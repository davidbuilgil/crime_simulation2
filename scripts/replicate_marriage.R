# -----------------------------------------------
# script to replicate Marriage_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# load raw data
marriage_raw <- read_excel(here("data","nomis_2020_12_31_151943.xlsx"), skip = 8)

# clean var names
names(marriage_raw) <- gsub(pattern = " ", replacement = "_", x = names(marriage_raw))

# renaming, drop missings, then create 'mean' measure
marriage_mean <- marriage_raw %>% 
  rename(OA = `2011_output_area`,
         Pop_marr = `All_usual_residents_aged_16+`,
         Married = Married,
         Civil = `In_a_registered_same-sex_civil_partnership`) %>% 
  drop_na(Pop_marr) %>% 
  mutate(Mean_married = (Married + Civil)/Pop_marr)

# save
write_csv(x = marriage_mean, path = here("data", "Marriage_by_OA_Manchester_replicate.csv"))
