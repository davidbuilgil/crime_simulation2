# -----------------------------------------------
# script to replicate Edu_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# load educ data we need to replicate. Note that we actually only need the 'mean' level 4 education measure,
# so many of the other columns are not required.
Edu_by_OA_Manchester <- read_excel(here("data","Edu_by_OA_Manchester.xlsx"))

# load raw data
educ_raw <- read_excel(here("data","nomis_2020_03_24_134734.xlsx"), skip = 8)

# clean var names
names(educ_raw) <- gsub(pattern = " ", replacement = "_", x = names(educ_raw))

# renaming, drop missings, then create 'mean' measure
educ_mean <- educ_raw %>% 
  rename(OA = `2011_output_area`,
         Pop_educ = `All_categories:_Highest_level_of_qualification`,
         Level4_educ = Level_4_qualifications_and_above) %>% 
  drop_na(Pop_educ) %>% 
  mutate(Mean_level4_edu = Level4_educ/Pop_educ)

# save
write_csv(x = educ_mean, path = here("data", "Edu_by_OA_Manchester_replicate.csv"))

