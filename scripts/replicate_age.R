# -----------------------------------------------
# script to replicate Age_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# load age data we need to replicate
Age_by_OA_Manchester <- read_excel(here("data","Age_by_OA_Manchester.xlsx"))

# load raw data
age_raw <- read_csv(here("data","nomis_2020_03_24_age_raw.csv"), skip = 8)

# clean var names
names(age_raw) <- gsub(pattern = " ", replacement = "_", x = names(age_raw))

# renaming
age_raw <- age_raw %>% 
  rename(OA = `2011_output_area`,
         Pop = `All_categories:_Age`) %>% 
  drop_na(Pop) %>% 
  arrange(OA)

# check N
N <- sum(age_raw$Pop, na.rm = T) # 503127
N

# create synthetic individual data frame with id col
ind.df <- data.frame(id = 1:N)

# create OA col, repeating the OA code as many times as there are residents in each
ind.df$OA <- rep.int(x = age_raw$OA, time = age_raw$Pop)

# arrange by OA to match age_raw
ind.df <- arrange(ind.df, OA)

# remove Pop for pivot longer, recode those under age of 1 to zero for mean calculation, then
# extract age as integer.
age_raw_long <- age_raw %>% 
  select(-Pop) %>% 
  pivot_longer(cols = Age_under_1:Age_100_and_over, names_to = "age", values_to = "count") %>%
  mutate(age =  if_else(age == "Age_under_1", true = "Age_0", false = age),
         age = parse_number(age))

# impute individual-level ages. This relies on the OA being arranged!
ind.df$age <- rep.int(x = age_raw_long$age, age_raw_long$count)

# plot age distribution
ggplot(data = ind.df) +
  geom_histogram(mapping = aes(x = age), bins = 40)

# calculate mean age of each OA
mean.age.df <- ind.df %>% 
  group_by(OA) %>% 
  summarise(mean_age_OA = mean(age), # new      (Code_v5b.r)
            Mean = mean(age),        # original (Code_v5a.r)
            sd_age_OA = sd(age),     # new      (Code_v5b.r)
            SD = sd(age))            # original (Code_v5a.r)
            # sd_age_OA = var(age),  # not used

# bring Pop back
mean.age.df <- age_raw %>% 
  select(OA, Pop) %>% 
  left_join(mean.age.df)

# save
write_csv(x = mean.age.df, path = here("data","Age_by_OA_Manchester_replicate.csv"))
