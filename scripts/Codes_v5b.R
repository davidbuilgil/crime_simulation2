########################################################
#                                                      #
# Simulation study levels of geography and crime data  #
#                                                      #
# Angelo Moretti and David Buil-Gil                    #
#                                                      #
# Edits: Sam Langton                                   #
#                                                      #
########################################################

# Load packages required.
library(DescTools)
library(MASS)
library(haven)
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(sf)
library(devtools)
library(maptools)
library(spdep)

rm(list = ls())

# Load data on age.
Age_by_OA_Manchester <- read_csv(here("data","Age_by_OA_Manchester_replicate.csv"))

# Calculate number of residents in Manchester.
N <- sum(Age_by_OA_Manchester$Pop)
N

# Calculate number of Output Areas (OA).
D <- nrow(Age_by_OA_Manchester)
D

# Create a tibble with one row for each synthetic resident in Manchester. 
# This will be filled with information later.
syn_res <- tibble(ID = 1:N)

# Assign each resident to an OA based on the size of the resident populations.
syn_res <- syn_res %>% 
  mutate(OA = rep.int(x = Age_by_OA_Manchester$OA, times = Age_by_OA_Manchester$Pop))

# Join the OA-level age data with the synthetic individual-level data.
syn_res_OA <- left_join(syn_res, Age_by_OA_Manchester, by = "OA")

# Create age for each resident based on this distribution.
# Seed is set for replication.
set.seed(1612)
syn_res_OA <- syn_res_OA %>% 
  mutate(Age = rnorm(n = N, mean = syn_res_OA$mean_age_OA, sd = syn_res_OA$sd_age_OA)) %>% 
  select(ID, OA, Age)

# Load data on sex.
Sex_by_OA_Manchester <- read_csv(here("data","Sex_by_OA_Manchester_replicate.csv"))

# Join the OA-level sex data with the synthetic individual-level data.
syn_res_OA <- left_join(syn_res_OA, Sex_by_OA_Manchester, by = "OA")

# Create sex for each resident based on this distribution.
syn_res_OA <- syn_res_OA %>% 
  mutate(Male = rbinom(n = N, size = 1, prob = Mean_male)) %>% 
  select(ID, OA, Age, Male)

# Load data on ethnicity.
Ethnicity_by_OA_Manchester <- read_csv(here("data","Ethnicity_by_OA_Manchester_replicate.csv"))

# Join the OA-level sex data with the synthetic individual-level data.
syn_res_OA <- left_join(syn_res_OA, Ethnicity_by_OA_Manchester, by = "OA")

# Create ethnicity for resident (white or not) based on this distribution.
syn_res_OA <- syn_res_OA %>% 
  mutate(White = rbinom(n = N, size = 1, prob = Mean_white)) %>% 
  select(ID, OA, Age, Male, White)

# Load data on income.
Income_by_OA_Manchester <- read_csv(here("data","Income_by_OA_Manchester_replicate.csv"))

# Join the OA-level income data with the synthetic individual-level data.
syn_res_OA <- left_join(syn_res_OA, Income_by_OA_Manchester, by = "OA")

# Create income for resident (income or not) based on this distribution.
syn_res_OA <- syn_res_OA %>% 
  mutate(No_income = rbinom(n = N, size = 1, prob = mean_no_income)) %>%
  select(ID, OA, Age, Male, White, No_income)

# Load data on income.
Edu_by_OA_Manchester <- read_csv(here("data","Edu_by_OA_Manchester_replicate.csv"))

# Join the OA-level education data with the synthetic individual-level data.
syn_res_OA <- left_join(syn_res_OA, Edu_by_OA_Manchester, by = "OA")

# Create education for resident (Level 4 or not) based on this distribution.
syn_res_OA <- syn_res_OA %>% 
  mutate(High_edu = rbinom(n = N, size = 1, prob = Mean_level4_edu)) %>%
  select(ID, OA, Age, Male, White, No_income, High_edu)

# Load in CSEW nvf data
load(here("data","csew_apr11mar12_nvf.Rdata"))

# Missings age for those over 120 years old.
# `ifelse()` used for missings as it handles NA (unlike if_else).
csew <- csew %>% 
  mutate(age = ifelse(test = age > 120, yes = NA, no = age)) 

# Recode sex female from 2 to 0
csew <- csew %>% 
  mutate(sex = if_else(condition = sex == 2, true = 0, false = sex))

# Recode ethnicity in CSEW
csew <- csew %>% 
  mutate(reseth = if_else(condition = reseth != 1, true = 0, false = reseth))

# Missings for some work2 categories, and recode.
# `ifelse()` used for missings as it handles NA (unlike if_else).
csew <- csew %>%
  mutate(work2 = ifelse(test = work2 > 3, yes = NA, no = work2), 
         work2 = if_else(condition = work2 == 2, true = 0, false = work2))

# Recode employment.
csew <- csew %>% 
  mutate(remploy = if_else(condition = remploy == 2 | remploy == 3, true = 1, false = 0))

# Missings for some educat2 categories, and recode.
# `ifelse()` used for missings as it handles NA (unlike if_else).
csew <- csew %>% 
  mutate(educat2 = ifelse(test = educat2 > 10, yes = NA, no = educat2),
         educat2 = if_else(condition = educat2 == 1 | educat2 == 2 | educat2 == 3, true = 1, false = 0))

# Data handling to crime categories: replace missings with zeros and then sum to create new categories.
csew <- csew %>% 
  replace_na(replace = list(nmotthef = 0, nmotstol = 0, ncardam  = 0, nbikthef = 0,
                            nprevthe = 0, nprevdam = 0, nprevtry = 0, nprevsto = 0,
                            nproside = 0, nprdefac = 0, nhomthef = 0, nyrhthef = 0,
                            nyrhodam = 0, nyrhotry = 0, nyrhosto = 0, nyroside = 0,
                            nyrdefac = 0, npersth  = 0, ntrypers = 0, noththef = 0,
                            ndelibda = 0, ndelibv  = 0, nthrevio = 0, nsexatt  = 0,
                            nhhldvio = 0)) %>% 
  mutate(vehicle    = nmotthef + nmotstol + ncardam + nbikthef,
         residence  = nprevthe + nprevdam + nprevtry + nprevsto + nproside + nprdefac +
           nhomthef + nyrhthef + nyrhodam + nyrhotry + nyrhosto + nyroside + nyrdefac,
         theft      = npersth  + ntrypers + noththef,
         theft_dam  = theft    + ndelibda,
         violence   = ndelibv  + nthrevio  + nsexatt + nhhldvio,
         all_crimes = vehicle  + residence + theft + ndelibda + violence)

# Run negative binomial models to generate estimates from CSEW.

model_vehicle <- glm.nb(vehicle ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_vehicle)
PseudoR2(model_vehicle)

model_residence <- glm.nb(residence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_residence)
PseudoR2(model_residence)

model_theft <- glm.nb(theft ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_theft)
PseudoR2(model_theft)

model_violence <- glm.nb(violence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_violence)
PseudoR2(model_violence)

model_all_crimes <- glm.nb(all_crimes ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_all_crimes)

# Combine estimates for each crime type, assigning to a vector.
# Vehicle crime
eta_vehicle <- model_vehicle$coefficients[1] +
  syn_res_OA$Age * model_vehicle$coefficients[2] +
  syn_res_OA$Male * model_vehicle$coefficients[3] +
  syn_res_OA$White * model_vehicle$coefficients[4] +
  syn_res_OA$No_income * model_vehicle$coefficients[5] +
  syn_res_OA$High_edu * model_vehicle$coefficients[6]

# Residence crime 
eta_residence <- model_residence$coefficients[1] +
  syn_res_OA$Age * model_residence$coefficients[2] +
  syn_res_OA$Male * model_residence$coefficients[3] +
  syn_res_OA$White * model_residence$coefficients[4] +
  syn_res_OA$No_income * model_residence$coefficients[5] +
  syn_res_OA$High_edu * model_residence$coefficients[6]

# Theft crime
eta_theft <- model_theft$coefficients[1] +
  syn_res_OA$Age * model_theft$coefficients[2] +
  syn_res_OA$Male * model_theft$coefficients[3] +
  syn_res_OA$White * model_theft$coefficients[4] +
  syn_res_OA$No_income * model_theft$coefficients[5] +
  syn_res_OA$High_edu * model_theft$coefficients[6]

# Violent crime
eta_violence <- model_violence$coefficients[1] +
  syn_res_OA$Age * model_violence$coefficients[2] +
  syn_res_OA$Male * model_violence$coefficients[3] +
  syn_res_OA$White * model_violence$coefficients[4] +
  syn_res_OA$No_income * model_violence$coefficients[5] +
  syn_res_OA$High_edu * model_violence$coefficients[6]

# Create distributions based on these estimates for the synthetic individual data.
syn_res_OA <-  syn_res_OA %>% 
  mutate(vehicle   = rnbinom(length(eta_vehicle)  , mu = exp(eta_vehicle)  , size = model_vehicle$theta),
         residence = rnbinom(length(eta_residence), mu = exp(eta_residence), size = model_residence$theta),
         theft     = rnbinom(length(eta_theft)    , mu = exp(eta_theft)    , size = model_theft$theta),
         violence  = rnbinom(length(eta_violence) , mu = exp(eta_violence) , size = model_violence$theta))

# Load CSEW vf data
load(here("data","csew_apr11mar12_vf.Rdata"))

# Subset only those synthetic individuals who were victimised for vehicle crime.
syn_res_OA_veh <- syn_res_OA %>%
  filter(vehicle != 0)

# Repeat individuals as many times as they were victimised.
syn_res_OA_veh <- as.data.frame(lapply(syn_res_OA_veh, rep, syn_res_OA_veh$vehicle))

# Recode so vehicle is 1, else crime types 0.
Data_vehicle <- syn_res_OA_veh %>% 
  mutate(vehicle   = 1,
         residence = 0,
         theft     = 0,
         violence  = 0)

# Subset only those  synthetic individuals who were victimised for residence crime.
syn_res_OA_res <- syn_res_OA %>%
  filter(residence != 0)

# Repeat individuals as many times as they were victimised.
syn_res_OA_res <- as.data.frame(lapply(syn_res_OA_res, rep, syn_res_OA_res$residence))

# Recode so residence is 1, else crime types 0.
Data_residence <- syn_res_OA_res %>% 
  mutate(vehicle   = 0,
         residence = 1,
         theft     = 0,
         violence  = 0)

# Subset only those  synthetic individuals who were victimised for theft crime.
syn_res_OA_the <- syn_res_OA %>%
  filter(theft != 0)

# Repeat individuals as many times as they were victimised.
syn_res_OA_the <- as.data.frame(lapply(syn_res_OA_the, rep, syn_res_OA_the$theft))

# Recode so theft is 1, else crime types 0.
Data_theft <- syn_res_OA_the %>% 
  mutate(vehicle   = 0,
         residence = 0,
         theft     = 1,
         violence  = 0)

# Subset only those  synthetic individuals who were victimised for violence crime.
syn_res_OA_vio <- syn_res_OA %>%
  filter(violence != 0)

# Repeat individuals as many times as they were victimised for violence.
syn_res_OA_vio <- as.data.frame(lapply(syn_res_OA_vio, rep, syn_res_OA_vio$violence))

# Recode so violence is 1, else crime types 0.
Data_violence <- syn_res_OA_vio %>% 
  mutate(vehicle   = 0,
         residence = 0,
         theft     = 0,
         violence  = 1)

# Join csew with csew_vf. Warnings are not problematic.
csew_vf <- left_join(csew_vf, csew, by = "rowlabel")

# Recode copsknow variable to binary for regression model.
csew_vf <- csew_vf %>% 
  mutate(copsknow = if_else(condition = copsknow == 2, true = 0, false = copsknow),
         copsknow = na_if(x = copsknow, 8),
         copsknow = na_if(x = copsknow, 9))

# Filter vehicle crime types.
csew_vf_vehicle <-  csew_vf %>% 
  filter(crimtype == 1 | crimtype == 2 | crimtype == 3 | crimtype == 4)

# Filter residence crime type.
csew_vf_residence <-  csew_vf %>% 
  filter(crimtype == 5  | crimtype == 6  | crimtype == 7  | crimtype == 8  |
           crimtype == 9  | crimtype == 10 | crimtype == 11 | crimtype == 12 |
           crimtype == 13 | crimtype == 14 | crimtype == 15 | crimtype == 16 |
           crimtype == 17)

# Filter theft crime type.
csew_vf_theft <-  csew_vf %>% 
  filter(crimtype == 18 | crimtype == 19 | crimtype == 20)

# Filter violence crime type.
csew_vf_violence <-  csew_vf %>% 
  filter(crimtype == 22 | crimtype == 23 | crimtype == 24 | crimtype == 25)

# Create GLM formula for predicting copsknow (dep. var.) with demographic variables (ind. var.).
glm_copsknow <- copsknow ~ age + sex + reseth + remploy + educat2

# Vehicle model.
model_repo_vehicle <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_vehicle)
summary(model_repo_vehicle)
PseudoR2(model_repo_vehicle)

# Residence model.
model_repo_residence <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_residence)
summary(model_repo_residence)
PseudoR2(model_repo_residence)

# Theft model.
model_repo_theft <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_theft)
summary(model_repo_theft)
PseudoR2(model_repo_theft)

# Violence model.
model_repo_violence <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_violence)
summary(model_repo_violence)
PseudoR2(model_repo_vehicle)

# Extract estimates for vehicle crime.
Data_vehicle <- Data_vehicle %>% 
  mutate(estimates = model_repo_vehicle$coefficients[1] +
           Data_vehicle$Age       * model_repo_vehicle$coefficients[2] +
           Data_vehicle$Male      * model_repo_vehicle$coefficients[3] +
           Data_vehicle$White     * model_repo_vehicle$coefficients[4] +
           Data_vehicle$No_income * model_repo_vehicle$coefficients[5] +
           Data_vehicle$High_edu  * model_repo_vehicle$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_vehicle), 1, exp_estimates))

# Check vehicle frequency distributions comparison.
table(csew_vf_vehicle$copsknow)
table(Data_vehicle$copsknow)

# Extract estimates for residence crime.
Data_residence <- Data_residence %>% 
  mutate(estimates = model_repo_residence$coefficients[1] +
           Data_residence$Age       * model_repo_residence$coefficients[2] +
           Data_residence$Male      * model_repo_residence$coefficients[3] +
           Data_residence$White     * model_repo_residence$coefficients[4] +
           Data_residence$No_income * model_repo_residence$coefficients[5] +
           Data_residence$High_edu  * model_repo_residence$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_residence), 1, exp_estimates))

# Check residence frequency distrbibutions comparison.
table(csew_vf_residence$copsknow)
table(Data_residence$copsknow)

# Extract theft for residence crime.
Data_theft <- Data_theft %>% 
  mutate(estimates = model_repo_theft$coefficients[1] +
           Data_theft$Age       * model_repo_theft$coefficients[2] +
           Data_theft$Male      * model_repo_theft$coefficients[3] +
           Data_theft$White     * model_repo_theft$coefficients[4] +
           Data_theft$No_income * model_repo_theft$coefficients[5] +
           Data_theft$High_edu  * model_repo_theft$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_theft), 1, exp_estimates))

# Check theft frequency distrbibutions comparison.
table(csew_vf_theft$copsknow)
table(Data_theft$copsknow)

# Extract violence for residence crime.
Data_violence <- Data_violence %>% 
  mutate(estimates = model_repo_violence$coefficients[1] +
           Data_violence$Age       * model_repo_violence$coefficients[2] +
           Data_violence$Male      * model_repo_violence$coefficients[3] +
           Data_violence$White     * model_repo_violence$coefficients[4] +
           Data_violence$No_income * model_repo_violence$coefficients[5] +
           Data_violence$High_edu  * model_repo_violence$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         copsknow = rbinom(nrow(Data_violence), 1, exp_estimates))

# Check violence frequency distrbibutions comparison.
table(csew_vf_violence$copsknow)
table(Data_violence$copsknow)

# Row bind each crime type data frame. Involves some factor -> character coercion for binding.
Data_crimes <- bind_rows(Data_vehicle, Data_residence, Data_theft, Data_violence)

# Load in census unit look-up table (OA, LSOA, MSOA, LAD).
OA_to_LAD <- read_csv(here("data","Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_Great_Britain__Classification_Version_2.csv"))

# Rename OA column and filter for Manchester.
OA_to_LAD <- OA_to_LAD %>% 
  rename(OA = OA11CD) %>% 
  filter(LAD17NM == "Manchester")

# Join to Data_crimes, remove unwaned columns, rename census units.
Data_crimes <- Data_crimes %>% 
  left_join(OA_to_LAD) %>% 
  rename(LSOA = LSOA11CD,
         MSOA = MSOA11CD,
         LAD  = LAD17CD) %>% 
  select(ID:violence, copsknow, LSOA, MSOA, LAD)

# Read in census look-up table(LSOA, Ward). Warnings not an issue.
LSOA_to_ward <- read_csv(here("data", "Lower_Layer_Super_Output_Area_2011_to_Ward_2018_Lookup_in_England_and_Wales_v3.csv"))

# Rename LSOA column and filter for Manchester.
LSOA_to_ward <- LSOA_to_ward %>% 
  rename(LSOA = LSOA11CD) %>% 
  filter(LAD18NM == "Manchester")

# Join to Data_crimes, remove unwaned columns, rename census units.
Data_crimes <- Data_crimes %>% 
  left_join(LSOA_to_ward) %>% 
  rename(WD = WD18CD) %>% 
  select(ID:LAD, WD)

# Save.
write_csv(x = Data_crimes, path = here("data","Data_crimes.csv"))

# For each spatial scale, tally the number of total crimes which ocurred (based on CSEW estimates) and
# the number of crimes which were known to police.

# OA
# Count number of crimes known to police for each OA.
crimes_OA_know <- Data_crimes %>%
  group_by(OA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of crimes per OA, merge with known to police.
crimes_OA <- Data_crimes %>%
  group_by(OA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(crimes_OA_know) %>% 
  mutate(unit = "OA") %>% 
  select(-OA)

# Count number of vehicle crimes known to police for each OA.
vehicle_OA_know <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(OA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of vehicle crimes per OA, merge with known to police.
vehicle_OA <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(OA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(vehicle_OA_know) %>% 
  mutate(unit = "OA") %>% 
  select(-OA)

# Count number of residence crimes known to police for each OA.
residence_OA_know <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(OA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of residence crimes per OA, merge with known to police.
residence_OA <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(OA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(residence_OA_know) %>% 
  mutate(unit = "OA") %>% 
  select(-OA)

# Count number of thefts known to police for each OA.
theft_OA_know <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(OA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of thefts per OA, merge with known to police.
theft_OA <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(OA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(theft_OA_know) %>% 
  mutate(unit = "OA") %>% 
  select(-OA)

# Count number of violent crimes known to police for each OA.
violent_OA_know <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(OA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of violent crimes per OA, merge with known to police.
violent_OA <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(OA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(violent_OA_know) %>% 
  mutate(unit = "OA") %>% 
  select(-OA)

# LSOA
# Count number of crimes known to police for each LSOA
crimes_LSOA_know <- Data_crimes %>%
  group_by(LSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of crimes per LSOA, merge with known to police.
crimes_LSOA <- Data_crimes %>%
  group_by(LSOA) %>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(crimes_LSOA_know) %>% 
  mutate(unit = "LSOA") %>% 
  select(-LSOA)

# Count number of vehicle crimes known to police for each LSOA.
vehicle_LSOA_know <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(LSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of vehicle crimes per LSOA, merge with known to police.
vehicle_LSOA <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(LSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(vehicle_LSOA_know) %>% 
  mutate(unit = "LSOA") %>% 
  select(-LSOA)

# Count number of residence crimes known to police for each LSOA.
residence_LSOA_know <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(LSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of residence crimes per LSOA, merge with known to police.
residence_LSOA <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(LSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(residence_LSOA_know) %>% 
  mutate(unit = "LSOA") %>% 
  select(-LSOA)

# Count number of thefts known to police for each LSOA.
theft_LSOA_know <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(LSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of thefts per LSOA, merge with known to police.
theft_LSOA <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(LSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(theft_LSOA_know) %>% 
  mutate(unit = "LSOA") %>% 
  select(-LSOA)

# Count number of violent crimes known to police for each LSOA.
violent_LSOA_know <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(LSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of violent crimes per LSOA, merge with known to police.
violent_LSOA <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(LSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(violent_LSOA_know) %>% 
  mutate(unit = "LSOA") %>% 
  select(-LSOA)

# MSOA
# Count number of crimes known to police for each MSOA.
crimes_MSOA_know <- Data_crimes %>%
  group_by(MSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of crimes per MSOA, merge with known to police'
crimes_MSOA <- Data_crimes %>%
  group_by(MSOA) %>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(crimes_MSOA_know) %>% 
  mutate(unit = "MSOA") %>% 
  select(-MSOA)

# Count number of vehicle crimes known to police for each MSOA.
vehicle_MSOA_know <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(MSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of vehicle crimes per MSOA, merge with known to police.
vehicle_MSOA <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(MSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(vehicle_MSOA_know) %>% 
  mutate(unit = "MSOA") %>% 
  select(-MSOA)

# Count number of residence crimes known to police for each MSOA.
residence_MSOA_know <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(MSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of residence crimes per MSOA, merge with known to police.
residence_MSOA <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(MSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(residence_MSOA_know) %>% 
  mutate(unit = "MSOA") %>% 
  select(-MSOA)

# Count number of thefts known to police for each MSOA.
theft_MSOA_know <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(MSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of thefts per MSOA, merge with known to police.
theft_MSOA <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(MSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(theft_MSOA_know) %>% 
  mutate(unit = "MSOA") %>% 
  select(-MSOA)

# Count number of violent crimes known to police for each MSOA.
violent_MSOA_know <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(MSOA) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of violent crimes per MSOA, merge with known to police.
violent_MSOA <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(MSOA)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(violent_MSOA_know) %>% 
  mutate(unit = "MSOA") %>% 
  select(-MSOA)

# Ward.
# Count number of crimes known to police for each Ward.
crimes_WD_know <- Data_crimes %>%
  group_by(WD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of crimes per ward, merge with known to police.
crimes_WD <- Data_crimes %>%
  group_by(WD) %>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(crimes_WD_know) %>% 
  mutate(unit = "WD") %>% 
  select(-WD)

# Count number of vehicle crimes known to police for each WD.
vehicle_WD_know <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(WD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of vehicle crimes per WD, merge with known to police.
vehicle_WD <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(WD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(vehicle_WD_know) %>% 
  mutate(unit = "WD") %>% 
  select(-WD)

# Count number of residence crimes known to police for each WD.
residence_WD_know <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(WD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of residence crimes per WD, merge with known to police.
residence_WD <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(WD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(residence_WD_know) %>% 
  mutate(unit = "WD") %>% 
  select(-WD)

# Count number of thefts known to police for each WD.
theft_WD_know <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(WD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of thefts per WD, merge with known to police.
theft_WD <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(WD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(theft_WD_know) %>% 
  mutate(unit = "WD") %>% 
  select(-WD)

# Count number of violent crimes known to police for each WD.
violent_WD_know <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(WD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of violent crimes per WD, merge with known to police.
violent_WD <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(WD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(violent_WD_know) %>% 
  mutate(unit = "WD") %>% 
  select(-WD)

# LAD.
# Count number of crimes known to police for each LAD.
crimes_LAD_know <- Data_crimes %>%
  group_by(LAD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of crimes per LAD, merge with known to police.
crimes_LAD <- Data_crimes %>%
  group_by(LAD) %>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(crimes_LAD_know) %>% 
  mutate(unit = "LAD") %>% 
  select(-LAD)

# Count number of vehicle crimes known to police for each LAD.
vehicle_LAD_know <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(LAD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of vehicle crimes per LAD, merge with known to police.
vehicle_LAD <- Data_crimes %>%
  filter(vehicle == 1) %>%
  group_by(LAD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(vehicle_LAD_know) %>% 
  mutate(unit = "LAD") %>% 
  select(-LAD)

# Count number of residence crimes known to police for each LAD.
residence_LAD_know <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(LAD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of residence crimes per LAD, merge with known to police.
residence_LAD <- Data_crimes %>%
  filter(residence == 1) %>%
  group_by(LAD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(residence_LAD_know) %>% 
  mutate(unit = "LAD") %>% 
  select(-LAD)

# Count number of thefts known to police for each LAD.
theft_LAD_know <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(LAD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of thefts per LAD, merge with known to police.
theft_LAD <- Data_crimes %>%
  filter(theft == 1) %>%
  group_by(LAD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(theft_LAD_know) %>% 
  mutate(unit = "LAD") %>% 
  select(-LAD)

# Count number of violent crimes known to police for each LAD.
violent_LAD_know <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(LAD) %>%
  summarise(known = sum(copsknow)) %>% 
  ungroup()

# Count number of violent crimes per LAD, merge with known to police.
violent_LAD <- Data_crimes %>%
  filter(violence == 1) %>%
  group_by(LAD)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n) %>%
  left_join(violent_LAD_know) %>% 
  mutate(unit = "LAD") %>% 
  select(-LAD)

# Bind together so descriptives easily generated (omit crimes_LAD as only 1 observation).
crimes_units <- bind_rows(crimes_OA, crimes_LSOA, crimes_MSOA, crimes_WD)
vehicle_units <- bind_rows(vehicle_OA, vehicle_LSOA, vehicle_MSOA, vehicle_WD)
residence_units <- bind_rows(residence_OA, residence_LSOA, residence_MSOA, residence_WD)
theft_units <- bind_rows(theft_OA, theft_LSOA, theft_MSOA, theft_WD)
violent_units <- bind_rows(violent_OA, violent_LSOA, violent_MSOA, violent_WD)

# Calculate RD% by crime type.
crimes_units_stats <- crimes_units %>% 
  mutate(RD     = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD))

vehicle_units_stats <- vehicle_units %>% 
  mutate(RD     = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD))

residence_units_stats <- residence_units %>% 
  mutate(RD     = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD))

theft_units_stats <- theft_units %>% 
  mutate(RD     = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD))

violent_units_stats <- violent_units %>% 
  mutate(RD     = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD))

# Re-prder factor for visualisation.
crimes_units_stats$unit <- factor(crimes_units_stats$unit, levels = c("OA", "LSOA", "MSOA", "WD"))
vehicle_units_stats$unit <- factor(vehicle_units_stats$unit, levels = c("OA", "LSOA", "MSOA", "WD"))
residence_units_stats$unit <- factor(residence_units_stats$unit, levels = c("OA", "LSOA", "MSOA", "WD"))
theft_units_stats$unit <- factor(theft_units_stats$unit, levels = c("OA", "LSOA", "MSOA", "WD"))
violent_units_stats$unit <- factor(violent_units_stats$unit, levels = c("OA", "LSOA", "MSOA", "WD"))

# Print descriptives.
crimes_units_stats %>% 
  group_by(unit) %>% 
  summarise(mean_RD    = mean(RD),
            min_RD     = min(RD),
            max_RD     = max(RD),
            var_RD     = var(RD),
            sd_RD      = sd(RD),
            med_abs_RD = median(abs_RD))

vehicle_units_stats %>% 
  group_by(unit) %>% 
  summarise(mean_RD    = mean(RD),
            min_RD     = min(RD),
            max_RD     = max(RD),
            var_RD     = var(RD),
            sd_RD      = sd(RD),
            med_abs_RD = median(abs_RD))

residence_units_stats %>% 
  group_by(unit) %>% 
  summarise(mean_RD    = mean(RD),
            min_RD     = min(RD),
            max_RD     = max(RD),
            var_RD     = var(RD),
            sd_RD      = sd(RD),
            med_abs_RD = median(abs_RD))

theft_units_stats %>% 
  group_by(unit) %>% 
  summarise(mean_RD    = mean(RD),
            min_RD     = min(RD),
            max_RD     = max(RD),
            var_RD     = var(RD),
            sd_RD      = sd(RD),
            med_abs_RD = median(abs_RD))

violent_units_stats %>% 
  group_by(unit) %>% 
  summarise(mean_RD    = mean(RD),
            min_RD     = min(RD),
            max_RD     = max(RD),
            var_RD     = var(RD),
            sd_RD      = sd(RD),
            med_abs_RD = median(abs_RD))

# Visualise distribution of abs_RD at each spatial scale (boxplot).
p1 <- ggplot(data = crimes_units_stats) +
  theme_bw() +
  geom_boxplot(mapping = aes(x = unit,  y = abs_RD, fill = unit), colour = "black") +
  labs(title = "RD% between all crimes and crimes known to police",
       y = "RD", x = "") +
  scale_fill_viridis_d(alpha = 0.7) +
  theme(plot.title = element_text(size = 10),
        legend.position = "none")
p1

# Save visualisations.
ggsave(plot = p1, filename = "visuals/RD_boxplot.png", width = 15, height = 12, unit = "cm")

# Calculate ARB.
crimes_units_ARB <- crimes_units %>% 
  mutate(RB  = ((all_crimes / known)-1),
         ARB = abs(RB))

# Print ARB.
crimes_units_ARB %>% 
  group_by(unit) %>% 
  summarise(mean_ARB    = mean(ARB),
            min_ARB     = min(ARB),
            max_ARB     = max(ARB),
            var_ARB     = var(ARB),
            sd_ARB      = sd(ARB))

# Load crimes known to Greater Manchester Police in 2011/12
GMP_all <- read.csv(here("data/GMP_all.csv"))

if (exists("GMP_all")){
  
  print("You have downloaded the dataset of crimes recorded by GMP from Github. We have just saved 3 hours of your life")
  
} else {
  
  # Load crimes known to Greater Manchester Police in 2011/12
  GMP_Apr11 <- read.csv(here("data/GMP_crimes", "2011-04-greater-manchester-street.csv"))
  GMP_May11 <- read.csv(here("data/GMP_crimes", "2011-05-greater-manchester-street.csv"))
  GMP_Jun11 <- read.csv(here("data/GMP_crimes", "2011-06-greater-manchester-street.csv"))
  GMP_Jul11 <- read.csv(here("data/GMP_crimes", "2011-07-greater-manchester-street.csv"))
  GMP_Aug11 <- read.csv(here("data/GMP_crimes", "2011-08-greater-manchester-street.csv"))
  GMP_Sep11 <- read.csv(here("data/GMP_crimes", "2011-09-greater-manchester-street.csv"))
  GMP_Oct11 <- read.csv(here("data/GMP_crimes", "2011-10-greater-manchester-street.csv"))
  GMP_Nov11 <- read.csv(here("data/GMP_crimes", "2011-11-greater-manchester-street.csv"))
  GMP_Dec11 <- read.csv(here("data/GMP_crimes", "2011-12-greater-manchester-street.csv"))
  GMP_Jan12 <- read.csv(here("data/GMP_crimes", "2012-01-greater-manchester-street.csv"))
  GMP_Feb12 <- read.csv(here("data/GMP_crimes", "2012-02-greater-manchester-street.csv"))
  GMP_Marc12 <- read.csv(here("data/GMP_crimes", "2012-03-greater-manchester-street.csv"))
  
  # Merge all crimes known to police in 2011/12
  GMP_all <- do.call("rbind", 
                     list(GMP_Apr11, GMP_May11, GMP_Jun11, GMP_Jul11, GMP_Aug11, 
                          GMP_Sep11, GMP_Oct11, GMP_Nov11, GMP_Dec11, GMP_Jan12, 
                          GMP_Feb12, GMP_Marc12))
  
  # Remove original crime datasets
  rm("GMP_Apr11", "GMP_May11", "GMP_Jun11", "GMP_Jul11", "GMP_Aug11", 
     "GMP_Sep11", "GMP_Oct11", "GMP_Nov11", "GMP_Dec11", "GMP_Jan12", 
     "GMP_Feb12", "GMP_Marc12")
  
  # Create new column with number 1 for all crimes
  GMP_all$num <- 1
  
  # Select only those in Manchester LAD
  GMP_all <- subset(GMP_all, LSOA.code %in% LSOA_to_ward$LSOA)
  
  # Install and load PostcodesioR to geocode GMP data
  #devtools::install_github("ropensci/PostcodesioR")
  library(PostcodesioR)
  
  # Create matrix
  GMP_postcodes <- matrix(NA, ncol = 1, nrow = nrow(GMP_all))
  
  # Convert coordinates into postcodes
  for (row in 1:nrow(GMP_all)) { 
    print(row)
    GMP_postcodes[row] <- reverse_geocoding(GMP_all$Longitude[row], GMP_all$Latitude[row], limit = 1)
  }
  
  # Select only data about postcodes
  GMP_postcodes2 <- lapply(GMP_postcodes, function(x) x[[1]])
  
  # Modify NULLs by NAs
  GMP_postcodes2 <- replace(GMP_postcodes2, GMP_postcodes2=="NULL", NA)
  
  # Unlist postcodes
  GMP_postcodes3 <- as.data.frame(unlist(GMP_postcodes2))
  
  # Rename the column before merging it into GMP dataset
  colnames(GMP_postcodes3) <- "postcode"
  
  # Merging postcodes into GMP data
  GMP_all$postcode <- GMP_postcodes3$postcode
  
  # Load vlookup of postcodes into output areas
  postcodes_vlookup <- read.csv("https://opendata.arcgis.com/datasets/80628f9289574ba4b39a76ca7830b7e9_0.csv")
  
  # Rename first column
  colnames(postcodes_vlookup)[1] <- "postcode"
  
  # Delete spaces from two datasets to enable vlookup
  postcodes_vlookup$postcode <- gsub('\\s+', '', postcodes_vlookup$postcode)
  GMP_all$postcode <- gsub('\\s+', '', GMP_all$postcode)
  
  # Join vlookup with GMP crimes
  GMP_all <- left_join(GMP_all, postcodes_vlookup, by = "postcode")
  
  # write dataset of crimes known to police
  write_csv(x = GMP_all, path = here("data","GMP_all.csv"))
  
}

# Delete all GMP crimes not recorded in Manchester.
GMP_all <- GMP_all %>% filter(ladnm == "Manchester")

# Recode categories to those used in the simulation.
GMP_all <- GMP_all %>%
  mutate(Crime.type=recode(Crime.type, 
                           "Anti-social behaviour"       = "other",
                           "Burglary"                    = "residence",
                           "Criminal damage and arson"   = "residence",
                           "Drugs"                       = "other",
                           "Other crime"                 = "other",
                           "Other theft"                 = "theft",
                           "Public disorder and weapons" = "other",
                           "Robbery"                     = "theft",
                           "Shoplifting"                 = "other",
                           "Vehicle crime"               = "vehicle",
                           "Violent crime"               = "violent"))

# Count crimes per Output Area (all crimes)
GMP_all_oa <- GMP_all %>%
  group_by(oa11) %>%
  rename(OA = oa11) %>%
  summarise(GMP = sum(num))

# Count vehicle crimes per Output Area
GMP_vehicle_oa <- GMP_all %>%
  filter(Crime.type == "vehicle") %>%
  group_by(oa11) %>%
  rename(OA = oa11) %>%
  summarise(GMP = sum(num))

# Count residence crimes per Output Area
GMP_residence_oa <- GMP_all %>%
  filter(Crime.type == "residence") %>%
  group_by(oa11) %>%
  rename(OA = oa11) %>%
  summarise(GMP = sum(num))

# Count property crimes per Output Area
GMP_theft_oa <- GMP_all %>%
  filter(Crime.type == "theft") %>%
  group_by(oa11) %>%
  rename(OA = oa11) %>%
  summarise(GMP = sum(num))

# Count violent crimes per Output Area
GMP_violent_oa <- GMP_all %>%
  filter(Crime.type == "violent") %>%
  group_by(oa11) %>%
  rename(OA = oa11) %>%
  summarise(GMP = sum(num))

# Count crimes per LSOA (all crimes)
GMP_all_lsoa <- GMP_all %>%
  group_by(lsoa11cd) %>%
  rename(LSOA = lsoa11cd) %>%
  summarise(GMP = sum(num))

# Count vehicle crimes per LSOA
GMP_vehicle_lsoa <- GMP_all %>%
  filter(Crime.type == "vehicle") %>%
  group_by(lsoa11cd) %>%
  rename(LSOA = lsoa11cd) %>%
  summarise(GMP = sum(num))

# Count residence crimes per LSOA
GMP_residence_lsoa <- GMP_all %>%
  filter(Crime.type == "residence") %>%
  group_by(lsoa11cd) %>%
  rename(LSOA = lsoa11cd) %>%
  summarise(GMP = sum(num))

# Count property crimes per LSOA
GMP_theft_lsoa <- GMP_all %>%
  filter(Crime.type == "theft") %>%
  group_by(lsoa11cd) %>%
  rename(LSOA = lsoa11cd) %>%
  summarise(GMP = sum(num))

# Count violent crimes per LSOA
GMP_violent_lsoa <- GMP_all %>%
  filter(Crime.type == "violent") %>%
  group_by(lsoa11cd) %>%
  rename(LSOA = lsoa11cd) %>%
  summarise(GMP = sum(num))

# Count crimes per MSOA (all crimes)
GMP_all_msoa <- GMP_all %>%
  group_by(msoa11cd) %>%
  rename(MSOA = msoa11cd) %>%
  summarise(GMP = sum(num))

# Count vehicle crimes per MSOA
GMP_vehicle_msoa <- GMP_all %>%
  filter(Crime.type == "vehicle") %>%
  group_by(msoa11cd) %>%
  rename(MSOA = msoa11cd) %>%
  summarise(GMP = sum(num))

# Count residence crimes per MSOA
GMP_residence_msoa <- GMP_all %>%
  filter(Crime.type == "residence") %>%
  group_by(msoa11cd) %>%
  rename(MSOA = msoa11cd) %>%
  summarise(GMP = sum(num))

# Count property crimes per MSOA
GMP_theft_msoa <- GMP_all %>%
  filter(Crime.type == "theft") %>%
  group_by(msoa11cd) %>%
  rename(MSOA = msoa11cd) %>%
  summarise(GMP = sum(num))

# Count violent crimes per MSOA
GMP_violent_msoa <- GMP_all %>%
  filter(Crime.type == "violent") %>%
  group_by(msoa11cd) %>%
  rename(MSOA = msoa11cd) %>%
  summarise(GMP = sum(num))

# Rename first column of spatial vlookup.
colnames(LSOA_to_ward)[1] <- "LSOA.code"

# Merge all GMP records with spatial vlookup.
GMP_all <- left_join(GMP_all, LSOA_to_ward, by = "LSOA.code")

# Count crimes per ward (all crimes)
GMP_all_ward <- GMP_all %>%
  group_by(WD18CD) %>%
  rename(WD = WD18CD) %>%
  summarise(GMP = sum(num))

# Count vehicle crimes per WD
GMP_vehicle_ward <- GMP_all %>%
  filter(Crime.type == "vehicle") %>%
  group_by(WD18CD) %>%
  rename(WD = WD18CD) %>%
  summarise(GMP = sum(num))

# Count residence crimes per WD
GMP_residence_ward <- GMP_all %>%
  filter(Crime.type == "residence") %>%
  group_by(WD18CD) %>%
  rename(WD = WD18CD) %>%
  summarise(GMP = sum(num))

# Count property crimes per WD
GMP_theft_ward <- GMP_all %>%
  filter(Crime.type == "theft") %>%
  group_by(WD18CD) %>%
  rename(WD = WD18CD) %>%
  summarise(GMP = sum(num))

# Count violent crimes per WD
GMP_violent_ward <- GMP_all %>%
  filter(Crime.type == "violent") %>%
  group_by(WD18CD) %>%
  rename(WD = WD18CD) %>%
  summarise(GMP = sum(num))

# Merge GMP data with simulated dataset by crime type and spatial scale.
crimes_OA_all <- left_join(crimes_OA_know, GMP_all_oa, by = "OA")
crimes_OA_vehicle <- left_join(vehicle_OA_know, GMP_vehicle_oa, by = "OA")
crimes_OA_residence <- left_join(residence_OA_know, GMP_residence_oa, by = "OA")
crimes_OA_theft <- left_join(theft_OA_know, GMP_theft_oa, by = "OA")
crimes_OA_violent <- left_join(violent_OA_know, GMP_violent_oa, by = "OA")

crimes_LSOA_all <- left_join(crimes_LSOA_know, GMP_all_lsoa, by = "LSOA")
crimes_LSOA_vehicle <- left_join(vehicle_LSOA_know, GMP_vehicle_lsoa, by = "LSOA")
crimes_LSOA_residence <- left_join(residence_LSOA_know, GMP_residence_lsoa, by = "LSOA")
crimes_LSOA_theft <- left_join(theft_LSOA_know, GMP_theft_lsoa, by = "LSOA")
crimes_LSOA_violent <- left_join(violent_LSOA_know, GMP_violent_lsoa, by = "LSOA")

crimes_MSOA_all <- left_join(crimes_MSOA_know, GMP_all_msoa, by = "MSOA")
crimes_MSOA_vehicle <- left_join(vehicle_MSOA_know, GMP_vehicle_msoa, by = "MSOA")
crimes_MSOA_residence <- left_join(residence_MSOA_know, GMP_residence_msoa, by = "MSOA")
crimes_MSOA_theft <- left_join(theft_MSOA_know, GMP_theft_msoa, by = "MSOA")
crimes_MSOA_violent <- left_join(violent_MSOA_know, GMP_violent_msoa, by = "MSOA")

crimes_WD_all <- left_join(crimes_WD_know, GMP_all_ward, by = "WD")
crimes_WD_vehicle <- left_join(vehicle_WD_know, GMP_vehicle_ward, by = "WD")
crimes_WD_residence <- left_join(residence_WD_know, GMP_residence_ward, by = "WD")
crimes_WD_theft <- left_join(theft_WD_know, GMP_theft_ward, by = "WD")
crimes_WD_violent <- left_join(violent_WD_know, GMP_violent_ward, by = "WD")

# Recode NAs as 0
crimes_OA_all[is.na(crimes_OA_all)] <- 0
crimes_OA_vehicle[is.na(crimes_OA_vehicle)] <- 0
crimes_OA_residence[is.na(crimes_OA_residence)] <- 0
crimes_OA_theft[is.na(crimes_OA_theft)] <- 0
crimes_OA_violent[is.na(crimes_OA_violent)] <- 0

crimes_LSOA_all[is.na(crimes_LSOA_all)] <- 0
crimes_LSOA_vehicle[is.na(crimes_LSOA_vehicle)] <- 0
crimes_LSOA_residence[is.na(crimes_LSOA_residence)] <- 0
crimes_LSOA_theft[is.na(crimes_LSOA_theft)] <- 0
crimes_LSOA_violent[is.na(crimes_LSOA_violent)] <- 0

crimes_MSOA_all[is.na(crimes_MSOA_all)] <- 0
crimes_MSOA_vehicle[is.na(crimes_MSOA_vehicle)] <- 0
crimes_MSOA_residence[is.na(crimes_MSOA_residence)] <- 0
crimes_MSOA_theft[is.na(crimes_MSOA_theft)] <- 0
crimes_MSOA_violent[is.na(crimes_MSOA_violent)] <- 0

crimes_WD_all[is.na(crimes_WD_all)] <- 0
crimes_WD_vehicle[is.na(crimes_WD_vehicle)] <- 0
crimes_WD_residence[is.na(crimes_WD_residence)] <- 0
crimes_WD_theft[is.na(crimes_WD_theft)] <- 0
crimes_WD_violent[is.na(crimes_WD_violent)] <- 0

# Spearman's rank correlations at the output area level.
cor.test(crimes_OA_all$known, crimes_OA_all$GMP, method = "spearman")
cor.test(crimes_OA_vehicle$known, crimes_OA_vehicle$GMP, method = "spearman")
cor.test(crimes_OA_residence$known, crimes_OA_residence$GMP, method = "spearman")
cor.test(crimes_OA_theft$known, crimes_OA_theft$GMP, method = "spearman")
cor.test(crimes_OA_violent$known, crimes_OA_violent$GMP, method = "spearman")

# Spearman's rank correlations at the LSOA level
cor.test(crimes_LSOA_all$known, crimes_LSOA_all$GMP, method = "spearman")
cor.test(crimes_LSOA_vehicle$known, crimes_LSOA_vehicle$GMP, method = "spearman")
cor.test(crimes_LSOA_residence$known, crimes_LSOA_residence$GMP, method = "spearman")
cor.test(crimes_LSOA_theft$known, crimes_LSOA_theft$GMP, method = "spearman")
cor.test(crimes_LSOA_violent$known, crimes_LSOA_violent$GMP, method = "spearman")

# Spearman's rank correlations at the MSOA level
cor.test(crimes_MSOA_all$known, crimes_MSOA_all$GMP, method = "spearman")
cor.test(crimes_MSOA_vehicle$known, crimes_MSOA_vehicle$GMP, method = "spearman")
cor.test(crimes_MSOA_residence$known, crimes_MSOA_residence$GMP, method = "spearman")
cor.test(crimes_MSOA_theft$known, crimes_MSOA_theft$GMP, method = "spearman")
cor.test(crimes_MSOA_violent$known, crimes_MSOA_violent$GMP, method = "spearman")

# Spearman's rank correlations at the ward level
cor.test(crimes_WD_all$known, crimes_WD_all$GMP, method = "spearman")
cor.test(crimes_WD_vehicle$known, crimes_WD_vehicle$GMP, method = "spearman")
cor.test(crimes_WD_residence$known, crimes_WD_residence$GMP, method = "spearman")
cor.test(crimes_WD_theft$known, crimes_WD_theft$GMP, method = "spearman")
cor.test(crimes_WD_violent$known, crimes_WD_violent$GMP, method = "spearman")

# Load proximity matrix of OAs in Manchester
lw_OA <- readRDS("data/lw_OA.RData")

if (exists("lw_OA")){
  
  print("We have already created the proximity matrix of output areas in Manchester")
  
} else {
  
  # Load Output Areas in shapefile
  OAs <- st_read("D:/PhD Manchester/Data/Geographies/OAs Manchester 2 (shp)/england_oac_2011.shp")
  
  # Select those output areas from Manchester LAD
  OAs <- subset(OAs, code %in% crimes_OA_all$OA)
  
  # Order areas by Output Area code
  OAs <- OAs[order(OAs$code), ]
  
  # Create proximity matrix at the output area level
  Sy1_OAs <- poly2nb(OAs, row.names = OAs$codes)
  card(Sy1_OAs)
  lw_OA <- nb2listw(Sy1_OAs)
  
  # Save proximity matrix
  saveRDS(lw_OA, file="data/lw_OA.RData")
  
}

# Correlation betwen LISA from GMP and our dataset of simulated crimes
LISA_OA_all_GMP <- localmoran(crimes_OA_all$GMP, lw_OA)
LISA_OA_all_sim <- localmoran(crimes_OA_all$known, lw_OA)
cor.test(LISA_OA_all_GMP, LISA_OA_all_sim, method = "spearman")

df1 <- as.data.frame(LISA_OA_all_GMP)
df2 <- as.data.frame(LISA_OA_all_sim)
cor.test(df1$Ii, df2$Ii, method = "pearson")

summary(df1$Ii)
summary(df2$Ii)

LISA_OA_vehicle_GMP <- localmoran(crimes_OA_vehicle$GMP, lw_OA)
LISA_OA_vehicle_sim <- localmoran(crimes_OA_vehicle$known, lw_OA)
cor.test(LISA_OA_vehicle_GMP , LISA_OA_vehicle_sim, method = "spearman")

LISA_OA_residence_GMP <- localmoran(crimes_OA_residence$GMP, lw_OA)
LISA_OA_residence_sim <- localmoran(crimes_OA_residence$known, lw_OA)
cor.test(LISA_OA_residence_GMP , LISA_OA_residence_sim, method = "spearman")

LISA_OA_theft_GMP <- localmoran(crimes_OA_theft$GMP, lw_OA)
LISA_OA_theft_sim <- localmoran(crimes_OA_theft$known, lw_OA)
cor.test(LISA_OA_theft_GMP , LISA_OA_theft_sim, method = "spearman")

LISA_OA_violent_GMP <- localmoran(crimes_OA_violent$GMP, lw_OA)
LISA_OA_violent_sim <- localmoran(crimes_OA_violent$known, lw_OA)
cor.test(LISA_OA_violent_GMP , LISA_OA_violent_sim, method = "spearman")

# Load proximity matrix of LSOAs in Manchester
lw_LSOA <- readRDS("data/lw_LSOA.RData")

if (exists("lw_LSOA")){
  
  print("We have already created the proximity matrix of LSOAs in Manchester")
  
} else {
  
  # Load LSOAs in shapefile
  LSOAs <- st_read("D:/PhD Manchester/Data/Geographies/LSOA Manchester 2/LSOA Manchester/Manchester.shp")
  
  # Select those LSOAs from Manchester LAD
  LSOAs <- subset(LSOAs, code %in% crimes_LSOA_all$LSOA)
  
  # Order areas by LSOA code
  LSOAs <- LSOAs[order(LSOAs$code), ]
  
  # Create proximity matrix at the LSOA level
  Sy1_LSOAs <- poly2nb(LSOAs, row.names = LSOAs$codes)
  card(Sy1_LSOAs)
  lw_LSOA <- nb2listw(Sy1_LSOAs)
  
  # Save proximity matrix
  saveRDS(lw_LSOA, file="data/lw_LSOA.RData")
  
}

# Correlation betwen LISA from GMP and our dataset of simulated crimes
LISA_LSOA_all_GMP <- localmoran(crimes_LSOA_all$GMP, lw_LSOA)
LISA_LSOA_all_sim <- localmoran(crimes_LSOA_all$known, lw_LSOA)
cor.test(LISA_LSOA_all_GMP, LISA_LSOA_all_sim, method = "spearman")

LISA_LSOA_vehicle_GMP <- localmoran(crimes_LSOA_vehicle$GMP, lw_LSOA)
LISA_LSOA_vehicle_sim <- localmoran(crimes_LSOA_vehicle$known, lw_LSOA)
cor.test(LISA_LSOA_vehicle_GMP , LISA_LSOA_vehicle_sim, method = "spearman")

LISA_LSOA_residence_GMP <- localmoran(crimes_LSOA_residence$GMP, lw_LSOA)
LISA_LSOA_residence_sim <- localmoran(crimes_LSOA_residence$known, lw_LSOA)
cor.test(LISA_LSOA_residence_GMP , LISA_LSOA_residence_sim, method = "spearman")

LISA_LSOA_theft_GMP <- localmoran(crimes_LSOA_theft$GMP, lw_LSOA)
LISA_LSOA_theft_sim <- localmoran(crimes_LSOA_theft$known, lw_LSOA)
cor.test(LISA_LSOA_theft_GMP , LISA_LSOA_theft_sim, method = "spearman")

LISA_LSOA_violent_GMP <- localmoran(crimes_LSOA_violent$GMP, lw_LSOA)
LISA_LSOA_violent_sim <- localmoran(crimes_LSOA_violent$known, lw_LSOA)
cor.test(LISA_LSOA_violent_GMP , LISA_LSOA_violent_sim, method = "spearman")

# Load proximity matrix of OAs in Manchester
lw_MSOA <- readRDS("data/lw_MSOA.RData")

if (exists("lw_MSOA")){
  
  print("We have already created the proximity matrix of MSOAs in Manchester")
  
} else {
  
  # Load MSOAs in shapefile
  MSOAs <- st_read("D:/PhD Manchester/Data/Geographies/MSOAs England (shp)/england_msoa_2011.shp")
  
  # Select those MSOAs from Manchester LAD
  MSOAs <- subset(MSOAs, code %in% crimes_MSOA_all$MSOA)
  
  # Order areas by MSOAs code
  MSOAs <- MSOAs[order(MSOAs$code), ]
  
  # Create proximity matrix at the MSOA level
  Sy1_MSOAs <- poly2nb(MSOAs, row.names = MSOAs$codes)
  card(Sy1_MSOAs)
  lw_MSOA <- nb2listw(Sy1_MSOAs)
  
  # Save proximity matrix
  saveRDS(lw_MSOA, file="data/lw_MSOA.RData")
  
}

# Correlation betwen LISA from GMP and our dataset of simulated crimes
LISA_MSOA_all_GMP <- localmoran(crimes_MSOA_all$GMP, lw_MSOA)
LISA_MSOA_all_sim <- localmoran(crimes_MSOA_all$known, lw_MSOA)
cor.test(LISA_MSOA_all_GMP, LISA_MSOA_all_sim, method = "spearman")

LISA_MSOA_vehicle_GMP <- localmoran(crimes_MSOA_vehicle$GMP, lw_MSOA)
LISA_MSOA_vehicle_sim <- localmoran(crimes_MSOA_vehicle$known, lw_MSOA)
cor.test(LISA_MSOA_vehicle_GMP , LISA_MSOA_vehicle_sim, method = "spearman")

LISA_MSOA_residence_GMP <- localmoran(crimes_MSOA_residence$GMP, lw_MSOA)
LISA_MSOA_residence_sim <- localmoran(crimes_MSOA_residence$known, lw_MSOA)
cor.test(LISA_MSOA_residence_GMP , LISA_MSOA_residence_sim, method = "spearman")

LISA_MSOA_theft_GMP <- localmoran(crimes_MSOA_theft$GMP, lw_MSOA)
LISA_MSOA_theft_sim <- localmoran(crimes_MSOA_theft$known, lw_MSOA)
cor.test(LISA_MSOA_theft_GMP , LISA_MSOA_theft_sim, method = "spearman")

LISA_MSOA_violent_GMP <- localmoran(crimes_MSOA_violent$GMP, lw_MSOA)
LISA_MSOA_violent_sim <- localmoran(crimes_MSOA_violent$known, lw_MSOA)
cor.test(LISA_MSOA_violent_GMP , LISA_MSOA_violent_sim, method = "spearman")

# Load proximity matrix of OAs in Manchester
lw_WD <- readRDS("data/lw_WD.RData")

if (exists("lw_WD")){
  
  print("We have already created the proximity matrix of wards in Manchester")
  
} else {
  
  # Load Wards in shapefile
  Wards <- st_read("D:/PhD Manchester/Data/Geographies/Wards UK/Wards_December_2018_Full_Clipped_Boundaries_GB.shp")
  
  # Select those Wards from Manchester LAD
  Wards <- subset(Wards, wd18cd %in% crimes_WD_all$WD)
  
  # Order areas by Ward code
  Wards <- Wards[order(Wards$wd18cd), ]
  
  # Create proximity matrix at the MSOA level
  Sy1_Wards <- poly2nb(Wards, row.names = Wards$wd18cd)
  card(Sy1_Wards)
  lw_Wards <- nb2listw(Sy1_Wards)
  
  # Save proximity matrix
  saveRDS(lw_Wards, file="data/lw_WD.RData")
  
}

# Correlation betwen LISA from GMP and our dataset of simulated crimes
LISA_WD_all_GMP <- localmoran(crimes_WD_all$GMP, lw_WD)
LISA_WD_all_sim <- localmoran(crimes_WD_all$known, lw_WD)
cor.test(LISA_WD_all_GMP, LISA_WD_all_sim, method = "spearman")

LISA_WD_vehicle_GMP <- localmoran(crimes_WD_vehicle$GMP, lw_WD)
LISA_WD_vehicle_sim <- localmoran(crimes_WD_vehicle$known, lw_WD)
cor.test(LISA_WD_vehicle_GMP , LISA_WD_vehicle_sim, method = "spearman")

LISA_WD_residence_GMP <- localmoran(crimes_WD_residence$GMP, lw_WD)
LISA_WD_residence_sim <- localmoran(crimes_WD_residence$known, lw_WD)
cor.test(LISA_WD_residence_GMP , LISA_WD_residence_sim, method = "spearman")

LISA_WD_theft_GMP <- localmoran(crimes_WD_theft$GMP, lw_WD)
LISA_WD_theft_sim <- localmoran(crimes_WD_theft$known, lw_WD)
cor.test(LISA_WD_theft_GMP , LISA_WD_theft_sim, method = "spearman")

LISA_WD_violent_GMP <- localmoran(crimes_WD_violent$GMP, lw_WD)
LISA_WD_violent_sim <- localmoran(crimes_WD_violent$known, lw_WD)
cor.test(LISA_WD_violent_GMP , LISA_WD_violent_sim, method = "spearman")

# Empirical evaluation of simulated dataset from CSEW data

# Create three agre groups - CSEW data.
csew$age_rec[csew$age < 36] <- "less35"
csew$age_rec[csew$age >= 36 & csew$age < 56] <- "36to55"
csew$age_rec[csew$age >= 56] <- "56more"

# Create three agre groups - simulated data.
syn_res_OA$age_rec[syn_res_OA$Age >= 16 & syn_res_OA$Age < 36] <- "less35"
syn_res_OA$age_rec[syn_res_OA$Age >= 36 & syn_res_OA$Age < 56] <- "36to55"
syn_res_OA$age_rec[syn_res_OA$Age >= 56] <- "56more"

# Mean of vehicle crime victimisations by age.
stats::weighted.mean(x = csew$vehicle[which(csew$age_rec == "less35")],
                     w = csew$IndivWgt[which(csew$age_rec == "less35")],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$age_rec == "less35")])

stats::weighted.mean(x = csew$vehicle[which(csew$age_rec == "36to55")],
                     w = csew$IndivWgt[which(csew$age_rec == "36to55")],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$age_rec == "36to55")])

stats::weighted.mean(x = csew$vehicle[which(csew$age_rec == "56more")],
                     w = csew$IndivWgt[which(csew$age_rec == "56more")],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$age_rec == "56more")])

# Mean of vehicle crime victimisations by sex.
stats::weighted.mean(x = csew$vehicle[which(csew$sex == 1)],
                     w = csew$IndivWgt[which(csew$sex == 1)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$Male == 1)])

stats::weighted.mean(x = csew$vehicle[which(csew$sex == 0)],
                     w = csew$IndivWgt[which(csew$sex == 0)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$Male == 0)])

# Mean of vehicle crime victimisations by ethnicity.
stats::weighted.mean(x = csew$vehicle[which(csew$reseth == 1)],
                     w = csew$IndivWgt[which(csew$reseth == 1)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$White == 1)])

stats::weighted.mean(x = csew$vehicle[which(csew$reseth == 0)],
                     w = csew$IndivWgt[which(csew$reseth == 0)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$White == 0)])

# Mean of vehicle crime victimisations by employment status.
stats::weighted.mean(x = csew$vehicle[which(csew$remploy == 1)],
                     w = csew$IndivWgt[which(csew$remploy == 1)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$No_income == 1)])

stats::weighted.mean(x = csew$vehicle[which(csew$remploy == 0)],
                     w = csew$IndivWgt[which(csew$remploy == 0)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$No_income == 0)])

# Mean of vehicle crime victimisations by education level.
stats::weighted.mean(x = csew$vehicle[which(csew$educat2 == 1)],
                     w = csew$IndivWgt[which(csew$educat2 == 1)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$High_edu == 1)])

stats::weighted.mean(x = csew$vehicle[which(csew$educat2 == 0)],
                     w = csew$IndivWgt[which(csew$educat2 == 0)],
                     na.rm = T)

mean(x = syn_res_OA$vehicle[which(syn_res_OA$High_edu == 0)])

# Mean of residence crime victimisations by age.
stats::weighted.mean(x = csew$residence[which(csew$age_rec == "less35")],
                     w = csew$IndivWgt[which(csew$age_rec == "less35")],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$age_rec == "less35")])

stats::weighted.mean(x = csew$residence[which(csew$age_rec == "36to55")],
                     w = csew$IndivWgt[which(csew$age_rec == "36to55")],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$age_rec == "36to55")])

stats::weighted.mean(x = csew$residence[which(csew$age_rec == "56more")],
                     w = csew$IndivWgt[which(csew$age_rec == "56more")],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$age_rec == "56more")])

# Mean of residence crime victimisations by sex.
stats::weighted.mean(x = csew$residence[which(csew$sex == 1)],
                     w = csew$IndivWgt[which(csew$sex == 1)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$Male == 1)])

stats::weighted.mean(x = csew$residence[which(csew$sex == 0)],
                     w = csew$IndivWgt[which(csew$sex == 0)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$Male == 0)])

# Mean of residence crime victimisations by ethnicity.
stats::weighted.mean(x = csew$residence[which(csew$reseth == 1)],
                     w = csew$IndivWgt[which(csew$reseth == 1)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$White == 1)])

stats::weighted.mean(x = csew$residence[which(csew$reseth == 0)],
                     w = csew$IndivWgt[which(csew$reseth == 0)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$White == 0)])

# Mean of residence crime victimisations by employment status.
stats::weighted.mean(x = csew$residence[which(csew$remploy == 1)],
                     w = csew$IndivWgt[which(csew$remploy == 1)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$No_income == 1)])

stats::weighted.mean(x = csew$residence[which(csew$remploy == 0)],
                     w = csew$IndivWgt[which(csew$remploy == 0)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$No_income == 0)])

# Mean of residence crime victimisations by education level.
stats::weighted.mean(x = csew$residence[which(csew$educat2 == 1)],
                     w = csew$IndivWgt[which(csew$educat2 == 1)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$High_edu == 1)])

stats::weighted.mean(x = csew$residence[which(csew$educat2 == 0)],
                     w = csew$IndivWgt[which(csew$educat2 == 0)],
                     na.rm = T)

mean(x = syn_res_OA$residence[which(syn_res_OA$High_edu == 0)])

# Mean of property crime victimisations by age.
stats::weighted.mean(x = csew$theft[which(csew$age_rec == "less35")],
                     w = csew$IndivWgt[which(csew$age_rec == "less35")],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$age_rec == "less35")])

stats::weighted.mean(x = csew$theft[which(csew$age_rec == "36to55")],
                     w = csew$IndivWgt[which(csew$age_rec == "36to55")],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$age_rec == "36to55")])

stats::weighted.mean(x = csew$theft[which(csew$age_rec == "56more")],
                     w = csew$IndivWgt[which(csew$age_rec == "56more")],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$age_rec == "56more")])

# Mean of property crime victimisations by sex.
stats::weighted.mean(x = csew$theft[which(csew$sex == 1)],
                     w = csew$IndivWgt[which(csew$sex == 1)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$Male == 1)])

stats::weighted.mean(x = csew$theft[which(csew$sex == 0)],
                     w = csew$IndivWgt[which(csew$sex == 0)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$Male == 0)])

# Mean of property crime victimisations by ethnic group.
stats::weighted.mean(x = csew$theft[which(csew$reseth == 1)],
                     w = csew$IndivWgt[which(csew$reseth == 1)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$White == 1)])

stats::weighted.mean(x = csew$theft[which(csew$reseth == 0)],
                     w = csew$IndivWgt[which(csew$reseth == 0)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$White == 0)])

# Mean of property crime victimisations by employment status.
stats::weighted.mean(x = csew$theft[which(csew$remploy == 1)],
                     w = csew$IndivWgt[which(csew$remploy == 1)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$No_income == 1)])

stats::weighted.mean(x = csew$theft[which(csew$remploy == 0)],
                     w = csew$IndivWgt[which(csew$remploy == 0)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$No_income == 0)])

# Mean of property crime victimisations by education level.
stats::weighted.mean(x = csew$theft[which(csew$educat2 == 1)],
                     w = csew$IndivWgt[which(csew$educat2 == 1)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$High_edu == 1)])

stats::weighted.mean(x = csew$theft[which(csew$educat2 == 0)],
                     w = csew$IndivWgt[which(csew$educat2 == 0)],
                     na.rm = T)

mean(x = syn_res_OA$theft[which(syn_res_OA$High_edu == 0)])

# Mean of violent crime victimisations by age.
stats::weighted.mean(x = csew$violence[which(csew$age_rec == "less35")],
                     w = csew$IndivWgt[which(csew$age_rec == "less35")],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$age_rec == "less35")])

stats::weighted.mean(x = csew$violence[which(csew$age_rec == "36to55")],
                     w = csew$IndivWgt[which(csew$age_rec == "36to55")],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$age_rec == "36to55")])

stats::weighted.mean(x = csew$violence[which(csew$age_rec == "56more")],
                     w = csew$IndivWgt[which(csew$age_rec == "56more")],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$age_rec == "56more")])

# Mean of violent crime victimisations by sex.
stats::weighted.mean(x = csew$violence[which(csew$sex == 1)],
                     w = csew$IndivWgt[which(csew$sex == 1)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$Male == 1)])

stats::weighted.mean(x = csew$violence[which(csew$sex == 0)],
                     w = csew$IndivWgt[which(csew$sex == 0)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$Male == 0)])

# Mean of violent crime victimisations by ethnic group.
stats::weighted.mean(x = csew$violence[which(csew$reseth == 1)],
                     w = csew$IndivWgt[which(csew$reseth == 1)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$White == 1)])

stats::weighted.mean(x = csew$violence[which(csew$reseth == 0)],
                     w = csew$IndivWgt[which(csew$reseth == 0)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$White == 0)])

# Mean of violent crime victimisations by employment status.
stats::weighted.mean(x = csew$violence[which(csew$remploy == 1)],
                     w = csew$IndivWgt[which(csew$remploy == 1)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$No_income == 1)])

stats::weighted.mean(x = csew$violence[which(csew$remploy == 0)],
                     w = csew$IndivWgt[which(csew$remploy == 0)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$No_income == 0)])

# Mean of violent crime victimisations by education level.
stats::weighted.mean(x = csew$violence[which(csew$educat2 == 1)],
                     w = csew$IndivWgt[which(csew$educat2 == 1)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$High_edu == 1)])

stats::weighted.mean(x = csew$violence[which(csew$educat2 == 0)],
                     w = csew$IndivWgt[which(csew$educat2 == 0)],
                     na.rm = T)

mean(x = syn_res_OA$violence[which(syn_res_OA$High_edu == 0)])

# Create three age groups for vehicle crime dataset.
csew_vf_vehicle$age_rec[csew_vf_vehicle$age < 36] <- "less35"
csew_vf_vehicle$age_rec[csew_vf_vehicle$age >= 36 & csew_vf_vehicle$age < 56] <- "36to55"
csew_vf_vehicle$age_rec[csew_vf_vehicle$age >= 56] <- "56more"

Data_crimes$age_rec[Data_crimes$Age >= 16 & Data_crimes$Age < 36] <- "less35"
Data_crimes$age_rec[Data_crimes$Age >= 36 & Data_crimes$Age < 56] <- "36to55"
Data_crimes$age_rec[Data_crimes$Age >= 56] <- "56more"

# Mean of crime reporting for vehicle crimes by age.
stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$age_rec == "less35")],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$age_rec == "less35")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle >= 1 &
                                      Data_crimes$age_rec == "less35")])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$age_rec == "36to55")],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$age_rec == "36to55")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$age_rec == "36to55")])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$age_rec == "56more")],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$age_rec == "56more")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$age_rec == "56more")])

# Mean of crime reporting for vehicle crimes by sex.
stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$sex == 1)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$sex == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$Male == 1)])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$sex == 0)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$sex == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$Male == 0)])

# Mean of crime reporting for vehicle crimes by ethnic group.
stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$reseth == 1)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$reseth == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$White == 1)])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$reseth == 0)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$reseth == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$White == 0)])

# Mean of crime reporting for vehicle crimes by employment status.
stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$remploy == 1)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$remploy == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$No_income == 1)])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$remploy == 0)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$remploy == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$No_income == 0)])

# Mean of crime reporting for vehicle crimes by education level.
stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$educat2 == 1)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$educat2 == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$High_edu == 1)])

stats::weighted.mean(x = csew_vf_vehicle$copsknow[which(csew_vf_vehicle$educat2 == 0)],
                     w = csew_vf_vehicle$IndivWgt.x[which(csew_vf_vehicle$educat2 == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$vehicle == 1 &
                                      Data_crimes$High_edu == 0)])

# Create three age groups for residence crime data.
csew_vf_residence$age_rec[csew_vf_residence$age < 36] <- "less35"
csew_vf_residence$age_rec[csew_vf_residence$age >= 36 & csew_vf_residence$age < 56] <- "36to55"
csew_vf_residence$age_rec[csew_vf_residence$age >= 56] <- "56more"

# Mean of crime reporting for residence crimes by age.
stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$age_rec == "less35")],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$age_rec == "less35")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence >= 1 &
                                      Data_crimes$age_rec == "less35")])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$age_rec == "36to55")],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$age_rec == "36to55")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$age_rec == "36to55")])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$age_rec == "56more")],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$age_rec == "56more")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$age_rec == "56more")])

# Mean of crime reporting for residence crimes by sex.
stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$sex == 1)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$sex == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$Male == 1)])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$sex == 0)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$sex == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$Male == 0)])

# Mean of crime reporting for residence crimes by ethnic group.
stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$reseth == 1)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$reseth == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$White == 1)])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$reseth == 0)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$reseth == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$White == 0)])

# Mean of crime reporting for residence crimes by employment status.
stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$remploy == 1)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$remploy == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$No_income == 1)])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$remploy == 0)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$remploy == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$No_income == 0)])

# Mean of crime reporting for residence crimes by education level.
stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$educat2 == 1)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$educat2 == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$High_edu == 1)])

stats::weighted.mean(x = csew_vf_residence$copsknow[which(csew_vf_residence$educat2 == 0)],
                     w = csew_vf_residence$IndivWgt.x[which(csew_vf_residence$educat2 == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$residence == 1 &
                                      Data_crimes$High_edu == 0)])

# Create three age groups for property crime data.
csew_vf_theft$age_rec[csew_vf_theft$age < 36] <- "less35"
csew_vf_theft$age_rec[csew_vf_theft$age >= 36 & csew_vf_theft$age < 56] <- "36to55"
csew_vf_theft$age_rec[csew_vf_theft$age >= 56] <- "56more"

# Mean of crime reporting for property crimes by age.
stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$age_rec == "less35")],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$age_rec == "less35")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft >= 1 &
                                      Data_crimes$age_rec == "less35")])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$age_rec == "36to55")],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$age_rec == "36to55")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$age_rec == "36to55")])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$age_rec == "56more")],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$age_rec == "56more")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$age_rec == "56more")])

# Mean of crime reporting for property crimes by sex.
stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$sex == 1)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$sex == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$Male == 1)])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$sex == 0)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$sex == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$Male == 0)])

# Mean of crime reporting for property crimes by ethnic group.
stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$reseth == 1)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$reseth == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$White == 1)])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$reseth == 0)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$reseth == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$White == 0)])

# Mean of crime reporting for property crimes by employment status.
stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$remploy == 1)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$remploy == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$No_income == 1)])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$remploy == 0)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$remploy == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$No_income == 0)])

# Mean of crime reporting for property crimes by education level.
stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$educat2 == 1)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$educat2 == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$High_edu == 1)])

stats::weighted.mean(x = csew_vf_theft$copsknow[which(csew_vf_theft$educat2 == 0)],
                     w = csew_vf_theft$IndivWgt.x[which(csew_vf_theft$educat2 == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$theft == 1 &
                                      Data_crimes$High_edu == 0)])

#Create three age groups for violent crimes data.
csew_vf_violence$age_rec[csew_vf_violence$age < 36] <- "less35"
csew_vf_violence$age_rec[csew_vf_violence$age >= 36 & csew_vf_violence$age < 56] <- "36to55"
csew_vf_violence$age_rec[csew_vf_violence$age >= 56] <- "56more"

# Mean of crime reporting for violent crimes by age.
stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$age_rec == "less35")],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$age_rec == "less35")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence >= 1 &
                                      Data_crimes$age_rec == "less35")])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$age_rec == "36to55")],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$age_rec == "36to55")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$age_rec == "36to55")])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$age_rec == "56more")],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$age_rec == "56more")],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$age_rec == "56more")])

# Mean of crime reporting for violent crimes by sex.
stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$sex == 1)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$sex == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$Male == 1)])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$sex == 0)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$sex == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$Male == 0)])

# Mean of crime reporting for violent crimes by ethnic group.
stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$reseth == 1)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$reseth == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$White == 1)])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$reseth == 0)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$reseth == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$White == 0)])

# Mean of crime reporting for violent crimes by employment status.
stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$remploy == 1)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$remploy == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$No_income == 1)])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$remploy == 0)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$remploy == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$No_income == 0)])

# Mean of crime reporting for violent crimes by education level.
stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$educat2 == 1)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$educat2 == 1)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$High_edu == 1)])

stats::weighted.mean(x = csew_vf_violence$copsknow[which(csew_vf_violence$educat2 == 0)],
                     w = csew_vf_violence$IndivWgt.x[which(csew_vf_violence$educat2 == 0)],
                     na.rm = T)

mean(x = Data_crimes$copsknow[which(Data_crimes$violence == 1 &
                                      Data_crimes$High_edu == 0)])

# CREATE TWO FIGURES (two maps in each)

# FIGURE 1. ALL CRIMES AND CRIMES KNOWN TO POLICE AT THE OUTPUT AREA LEVEL (SIMULATED DATA)
# Details of two maps: 
#left map: number of crimes (all) by output area. coropleth map divided by quartiles
#right map: number of crimes known to police by output area. coropleth map divided by quartiles

# FIGURE 1. ALL CRIMES AND THOSE KNOWN TO POLICE AT THE MSOA LEVEL (SIMULATED DATA)
# Details of two maps: 
#left map: number of crimes (all) by MSOA. coropleth map divided by quartiles
#right map: number of crimes known to police by MSOA. coropleth map divided by quartiles