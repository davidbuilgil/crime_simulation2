# -----------------------------------------------
# script to replicate Income_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(readr)

# load income data we need to replicate.
Income_by_OA_Manchester <- read_excel(here("data","Income_by_OA_Manchester.xlsx"))

# load raw data
income_raw <- read_excel(here("data","nomis_2020_03_24_134158.xlsx"), skip = 8)

# clean var names
names(income_raw) <- gsub(pattern = " ", replacement = "_", x = names(income_raw))

# renaming, dropping missings pop and arranging OAs
income_mean <-  income_raw %>% 
  rename(OA = `2011_output_area`,
         Pop_econ = `All_categories:_Economic_activity`,
         unempl = `Economically_active:_Unemployed`,
         student = `Economically_active:_Full-time_student`,
         inactive = `Economically_inactive:_Total`) %>% 
  mutate(sum_no_income = unempl + student + inactive,
         mean_no_income = sum_no_income/Pop_econ) # old: mean_not_income

# save
write_csv(x = income_mean, path = here("data", "Income_by_OA_Manchester_replicate.csv"))


