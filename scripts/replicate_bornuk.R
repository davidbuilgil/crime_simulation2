# -----------------------------------------------
# script to replicate BornUK_by_OA_Manchester.xlsx 
# data was originally generated outside of R
# -----------------------------------------------

# load packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)
library(readr)

# load raw data
bornuk_raw <- read_excel(here("data","nomis_2020_12_31_151501.xlsx"), skip = 8)

# clean var names
names(bornuk_raw) <- gsub(pattern = " ", replacement = "_", x = names(bornuk_raw))

# renaming, drop missings, then create 'mean' measure
bornuk_mean <- bornuk_raw %>% 
  rename(OA = `2011_output_area`,
         Pop = All_usual_residents,
         UK = United_Kingdom) %>% 
  drop_na(Pop) %>% 
  mutate(Mean_bornUK = UK/Pop)

# save
write_csv(x = bornuk_mean, path = here("data", "BornUK_by_OA_Manchester_replicate.csv"))
