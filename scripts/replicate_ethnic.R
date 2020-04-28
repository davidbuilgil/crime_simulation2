# -----------------------------------------------
# script to replicate Ethnicity_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)

# load ethnic data we need to replicate.
Ethnicity_by_OA_Manchester <- read_excel(here("data","Ethnicity_by_OA_Manchester.xlsx"))

# load raw data
ethnic_raw <- read_excel(here("data","nomis_2020_03_24_132533.xlsx"), skip = 8)

# clean var names
names(ethnic_raw) <- gsub(pattern = " ", replacement = "_", x = names(ethnic_raw))

# renaming, dropping missings pop and arranging OAs
ethnic_raw <- ethnic_raw %>% 
  rename(OA = `2011_output_area`,
         Pop = `All_usual_residents`) %>% 
  mutate(Non_white = Pop-White) %>% 
  drop_na(Pop) %>% 
  arrange(OA)

# check N
N <- sum(ethnic_raw$Pop, na.rm = T) # 503127
N

# create synthetic individual data frame with id col
ind.df <- data.frame(id = 1:N)

# create OA col, repeating the OA code as many times as there are people in each
ind.df$OA <- rep.int(x = ethnic_raw$OA, time = ethnic_raw$Pop)

# arrange by OA to match ethnic_raw
ind.df <- arrange(ind.df, OA)

# pivot to get a binary variable for white or not
ethnic_raw_long <- ethnic_raw %>% 
  select(-Pop) %>% 
  pivot_longer(cols = White:Non_white, names_to = "ethnic", values_to = "count") %>% 
  mutate(ethnic = if_else(ethnic == "White", true = 1, false = 0))

# impute individual-level ethnic counts fo each OA. This relies on the OA being arranged!
ind.df$white <- rep.int(x = ethnic_raw_long$ethnic, ethnic_raw_long$count)

# calculate mean etc white of each OA
mean.white.df <- ind.df %>% 
  group_by(OA) %>% 
  summarise(Mean_white = mean(white), 
            Variance_white = var(white))
            # SD = sd(white))   # no SD used

# Get pop and White back
mean.white.df <- ethnic_raw %>%
  select(OA, Pop, White) %>% 
  left_join(mean.white.df)

# save
write_csv(x = mean.white.df, path = here("data","Ethnicity_by_OA_Manchester_replicate.csv"))  

