# -----------------------------------------------
# script to replicate Sex_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)

# load sex data we need to replicate
Sex_by_OA_Manchester <- read_excel(here("data","Sex_by_OA_Manchester.xlsx"))

# load raw data
sex_raw <- read_excel(here("data","nomis_2020_03_24_130204.xlsx"), skip = 8)

# clean var names
names(sex_raw) <- gsub(pattern = " ", replacement = "_", x = names(sex_raw))

# renaming
sex_raw <- sex_raw %>% 
  rename(OA = `2011_output_area`,
         Pop = `All_usual_residents`) %>% 
  drop_na(Pop) %>% 
  arrange(OA)

# check N
N <- sum(sex_raw$Pop, na.rm = T) # 503127
N

# create synthetic individual data frame with id col
ind.df <- data.frame(id = 1:N)

# create OA col, repeating the OA code as many times as there are residents in each
ind.df$OA <- rep.int(x = sex_raw$OA, time = sex_raw$Pop)

# arrange by OA to match sex_raw
ind.df <- arrange(ind.df, OA)

# remove Pop for pivot, then pivot long
sex_raw_long <- sex_raw %>% 
  select(-Pop) %>% 
  pivot_longer(cols = Males:Females, names_to = "sex", values_to = "count") %>% 
  mutate(sex = if_else(sex == "Males", true = 1, false = 0))

# impute individual-level sex counts fo each OA. This relies on the OA being arranged!
ind.df$sex <- rep.int(x = sex_raw_long$sex, sex_raw_long$count)

# calculate mean etc sex of each OA
mean.sex.df <- ind.df %>% 
  group_by(OA) %>% 
  summarise(Mean_male = mean(sex),
            Variance_male = var(sex),
            SD_male = sd(sex))

# bring Pop back
mean.sex.df <- sex_raw %>% 
  select(OA, Pop, Males, Females) %>% 
  left_join(mean.sex.df)


# save
write_csv(x = mean.sex.df, path = here("data","Sex_by_OA_Manchester_replicate.csv"))

