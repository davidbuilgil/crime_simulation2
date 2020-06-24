########################################################
#                                                      #
# Simulation study levels of geography and crime data  #
#                                                      #
# Angelo Moretti and David Buil-Gil                    #
#                                                      #
# Edits: Sam Langton                                   #
#                                                      #
########################################################

# To see everything in Console.
options(max.print=999999)

# Clear Global Environment.
rm(list = ls())

# Load packages required.
packages <- c("cowplot", "purrr"    , "devtools", "maptools",
              "spdep"  , "DescTools", "MASS"    , "haven"   , 
              "here"   , "readr"    , "dplyr"   , "tidyr"   , 
              "forcats", "ggplot2"  , "sf"      , "classInt",
              "rsq"    , "scales")

lapply(packages, require, character.only = TRUE)

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
set.seed(500)
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
  mutate(vehicle    = nmotthef + nmotstol + ncardam,
         residence  = nprevthe + nprevdam + nprevtry + nprevsto + nproside + nprdefac +
                      nhomthef + nyrhthef + nyrhodam + nyrhotry + nyrhosto + nyroside + nyrdefac,
         theft      = npersth  + ntrypers + noththef + nbikthef,
         theft_dam  = theft    + ndelibda,
         violence   = ndelibv  + nthrevio  + nsexatt + nhhldvio,
         all_crimes = vehicle  + residence + theft + ndelibda + violence)

# Run negative binomial models to generate estimates from CSEW.

model_vehicle <- glm.nb(vehicle ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_vehicle)
#PseudoR2(model_vehicle)
rsq.n(model_vehicle)

model_residence <- glm.nb(residence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_residence)
#PseudoR2(model_residence)
rsq.n(model_residence)

model_theft <- glm.nb(theft ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_theft)
#PseudoR2(model_theft)
rsq.n(model_theft)

model_violence <- glm.nb(violence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_violence)
#PseudoR2(model_violence)
rsq.n(model_violence)

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

# Recode victarea variable to binary for regression model.
csew_vf <- csew_vf %>% 
  mutate(victarea = if_else(condition = victarea == 2, true = 0, false = victarea),
         victarea = na_if(x = victarea, 8),
         victarea = na_if(x = victarea, 9))
  
# Filter vehicle crime types.
csew_vf_vehicle <-  csew_vf %>% 
  filter(crimtype == 1 | crimtype == 2 | crimtype == 3)

# Filter residence crime type.
csew_vf_residence <-  csew_vf %>% 
  filter(crimtype == 5  | crimtype == 6  | crimtype == 7  | crimtype == 8  |
         crimtype == 9  | crimtype == 10 | crimtype == 11 | crimtype == 12 |
         crimtype == 13 | crimtype == 14 | crimtype == 15 | crimtype == 16 |
         crimtype == 17)

# Filter theft crime type.
csew_vf_theft <-  csew_vf %>% 
  filter(crimtype == 18 | crimtype == 19 | crimtype == 20 | crimtype == 4)

# Filter violence crime type.
csew_vf_violence <-  csew_vf %>% 
  filter(crimtype == 22 | crimtype == 23 | crimtype == 24 | crimtype == 25)

# Create GLM formula for predicting copsknow (dep. var.) with demographic variables (ind. var.).
glm_copsknow <- copsknow ~ age + sex + reseth + remploy + educat2

# Vehicle model.
model_repo_vehicle <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_vehicle)
summary(model_repo_vehicle)
#PseudoR2(model_repo_vehicle)
rsq.n(model_repo_vehicle)

# Residence model.
model_repo_residence <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_residence)
summary(model_repo_residence)
#PseudoR2(model_repo_residence)
rsq.n(model_repo_residence)

# Theft model.
model_repo_theft <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_theft)
summary(model_repo_theft)
#PseudoR2(model_repo_theft)
rsq.n(model_repo_theft)

# Violence model.
model_repo_violence <- glm(formula = glm_copsknow, family = binomial(link = "logit"), data = csew_vf_violence)
summary(model_repo_violence)
#PseudoR2(model_repo_vehicle)
rsq.n(model_repo_vehicle)

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

# Create GLM formula for predicting victarea (dep. var.) with demographic variables (ind. var.).
glm_victarea <- victarea ~ age + sex + reseth + remploy + educat2

# Vehicle model.
model_area_vehicle <- glm(formula = glm_victarea, family = binomial(link = "logit"), data = csew_vf_vehicle)
summary(model_area_vehicle)
#PseudoR2(model_area_vehicle)
rsq.n(model_area_vehicle)

# Residence model.
model_area_residence <- glm(formula = glm_victarea, family = binomial(link = "logit"), data = csew_vf_residence)
summary(model_area_residence)
#PseudoR2(model_area_residence)
rsq.n(model_area_residence)

# Theft model.
model_area_theft <- glm(formula = glm_victarea, family = binomial(link = "logit"), data = csew_vf_theft)
summary(model_area_theft)
#PseudoR2(model_area_theft)
rsq.n(model_area_theft)

# Violence model.
model_area_violence <- glm(formula = glm_victarea, family = binomial(link = "logit"), data = csew_vf_violence)
summary(model_area_violence)
#PseudoR2(model_area_violence)
rsq.n(model_area_violence)

# Extract estimates for vehicle crime.
Data_vehicle <- Data_vehicle %>% 
  mutate(estimates = model_area_vehicle$coefficients[1] +
           Data_vehicle$Age       * model_area_vehicle$coefficients[2] +
           Data_vehicle$Male      * model_area_vehicle$coefficients[3] +
           Data_vehicle$White     * model_area_vehicle$coefficients[4] +
           Data_vehicle$No_income * model_area_vehicle$coefficients[5] +
           Data_vehicle$High_edu  * model_area_vehicle$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         victarea = rbinom(nrow(Data_vehicle), 1, exp_estimates))

# Check vehicle frequency distributions comparison.
table(csew_vf_vehicle$victarea)
table(Data_vehicle$victarea)

# Extract estimates for residence crime.
Data_residence <- Data_residence %>% 
  mutate(estimates = model_area_residence$coefficients[1] +
           Data_residence$Age       * model_area_residence$coefficients[2] +
           Data_residence$Male      * model_area_residence$coefficients[3] +
           Data_residence$White     * model_area_residence$coefficients[4] +
           Data_residence$No_income * model_area_residence$coefficients[5] +
           Data_residence$High_edu  * model_area_residence$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         victarea = rbinom(nrow(Data_residence), 1, exp_estimates))

# Check residence frequency distrbibutions comparison.
table(csew_vf_residence$victarea)
table(Data_residence$victarea)

# Extract theft for residence crime.
Data_theft <- Data_theft %>% 
  mutate(estimates = model_area_theft$coefficients[1] +
           Data_theft$Age       * model_area_theft$coefficients[2] +
           Data_theft$Male      * model_area_theft$coefficients[3] +
           Data_theft$White     * model_area_theft$coefficients[4] +
           Data_theft$No_income * model_area_theft$coefficients[5] +
           Data_theft$High_edu  * model_area_theft$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         victarea = rbinom(nrow(Data_theft), 1, exp_estimates))

# Check theft frequency distrbibutions comparison.
table(csew_vf_theft$victarea)
table(Data_theft$victarea)

# Extract violence for residence crime.
Data_violence <- Data_violence %>% 
  mutate(estimates = model_area_violence$coefficients[1] +
           Data_violence$Age       * model_area_violence$coefficients[2] +
           Data_violence$Male      * model_area_violence$coefficients[3] +
           Data_violence$White     * model_area_violence$coefficients[4] +
           Data_violence$No_income * model_area_violence$coefficients[5] +
           Data_violence$High_edu  * model_area_violence$coefficients[6],
         exp_estimates = exp(estimates) / (1 + exp(estimates)),
         victarea = rbinom(nrow(Data_violence), 1, exp_estimates))

# Check violence frequency distrbibutions comparison.
table(csew_vf_violence$victarea)
table(Data_violence$victarea)

# Row bind each crime type data frame. Involves some factor -> character coercion for binding.
Data_crimes <- bind_rows(Data_vehicle, Data_residence, Data_theft, Data_violence)

# Select only crimes that happen in local area.
Data_crimes <- Data_crimes %>%
  filter(victarea == 1)

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
# write_csv(x = Data_crimes, path = here("data","Data_crimes.csv"))

# For each spatial scale, tally the number of total crimes which ocurred (based on CSEW estimates) and
# the number of crimes which were known to police.

# Split Data_crimes into a list for loop. Each one is renamed 'unit' so the loop function is simple later.
# We use this again later so it's written as a function now.
split_fun <- function(x){
  list(x %>% rename(unit = OA)  ,
       x %>% rename(unit = LSOA),
       x %>% rename(unit = MSOA),
       x %>% rename(unit = WD)  ,
       x %>% rename(unit = LAD))
}

crimes_known_list <- split_fun(Data_crimes)

# Function for aggregating by unit.
crimes_known_fun <- function(x) {
  x %>%
    group_by(unit) %>%
    summarise(known = sum(copsknow)) %>% 
    ungroup()
}

# Loop aggregtion through list, so we get counts known to police for each spatial scale.
crimes_known_agg_list <- lapply(crimes_known_list, crimes_known_fun)

# Function to tally number of crimes (estimated from CSEW) per unit.
crimes_fun <- function(x){
x %>%
  group_by(unit)%>%
  tally() %>%
  ungroup() %>% 
  rename(all_crimes = n)
}

# Loop aggregation through list, so we get the crimes counts for each spatial scale.
crimes_list <- lapply(crimes_known_list, crimes_fun)

# Merge each together
crimes_known_full_list <- map2(crimes_list, crimes_known_agg_list, left_join)

# Name each element of list.
unit_names <- c("OA","LSOA","MSOA","WD","LAD")
names(crimes_known_full_list) <- unit_names

# Now we do the same but for each crime type.
crimes_Data_vehicle   <- Data_crimes %>% filter(vehicle == 1)
crimes_Data_residence <- Data_crimes %>% filter(residence == 1)
crimes_Data_theft     <- Data_crimes %>% filter(theft == 1)
crimes_Data_violence  <- Data_crimes %>% filter(violence == 1)

# Split function on each crime type df.
crimes_known_vehicle_list   <- split_fun(crimes_Data_vehicle)
crimes_known_residence_list <- split_fun(crimes_Data_residence)
crimes_known_theft_list     <- split_fun(crimes_Data_theft)
crimes_known_violence_list  <- split_fun(crimes_Data_violence)

# # Loop through lists, so we get counts known to police for each spatial scale, for each crime type.
crimes_known_vehicle_agg_list   <- lapply(crimes_known_vehicle_list  , crimes_known_fun)
crimes_known_residence_agg_list <- lapply(crimes_known_residence_list, crimes_known_fun)
crimes_known_theft_agg_list     <- lapply(crimes_known_theft_list    , crimes_known_fun)
crimes_known_violence_agg_list  <- lapply(crimes_known_violence_list , crimes_known_fun)

# Loop through lists, so we get the crimes counts for each spatial scale, for each crime type.
crimes_vehicle_list   <- lapply(crimes_known_vehicle_list, crimes_fun)
crimes_residence_list <- lapply(crimes_known_residence_list, crimes_fun)
crimes_theft_list     <- lapply(crimes_known_theft_list, crimes_fun)
crimes_violence_list  <- lapply(crimes_known_violence_list, crimes_fun)

# Merge each together
crimes_known_vehicle_full_list   <- map2(crimes_vehicle_list  , crimes_known_vehicle_agg_list  , left_join)
crimes_known_residence_full_list <- map2(crimes_residence_list, crimes_known_residence_agg_list, left_join)
crimes_known_theft_full_list     <- map2(crimes_theft_list    , crimes_known_theft_agg_list    , left_join)
crimes_known_violence_full_list  <- map2(crimes_violence_list , crimes_known_violence_agg_list , left_join)

# Name element of each list
names(crimes_known_vehicle_full_list)   <- unit_names
names(crimes_known_residence_full_list) <- unit_names
names(crimes_known_theft_full_list)     <- unit_names
names(crimes_known_violence_full_list)  <- unit_names

# Bind together so descriptives easily generated (omit crimes_LAD as only 1 observation).
crimes_units    <- bind_rows(crimes_known_full_list           , .id = "unit_type")
vehicle_units   <- bind_rows(crimes_known_vehicle_full_list   , .id = "unit_type")
residence_units <- bind_rows(crimes_known_residence_full_list , .id = "unit_type")
theft_units     <- bind_rows(crimes_known_theft_full_list     , .id = "unit_type")
violent_units   <- bind_rows(crimes_known_violence_full_list  , .id = "unit_type")

## Calculate RD% by crime type.
# Function for statistics, factor reordering.
rd_fun <- function(x) {
  x %>%
    mutate(RD = ((known - all_crimes) / all_crimes) * 100,
       abs_RD = abs(RD), 
       unit_type = fct_relevel(unit_type, "OA", "LSOA", "MSOA", "WD"))
    
}

# Compile into list and name
units_list <- list(crimes_units, vehicle_units, residence_units, theft_units, violent_units)
names(units_list) <- c("all_crimes", "vehicle", "residence", "theft", "violent")

# Loop funtion thru list
rd_stats_list <- lapply(units_list, rd_fun)

# Descriptives function
desc_fun <- function(x){
  x %>% 
    group_by(unit_type) %>% 
    summarise(mean_RD    = mean(abs_RD),
              min_RD     = min(abs_RD),
              max_RD     = max(abs_RD),
              var_RD     = var(abs_RD),
              sd_RD      = sd(abs_RD),
              med_abs_RD = median(abs_RD))
}

# Print descriptives.
lapply(rd_stats_list, desc_fun)

# Visualise boxplot for each all crimes and by crime type. First remove LAD because no distribution to speak of.
rd_stats_list <- lapply(rd_stats_list, function(x){filter(x, unit_type != "LAD")})

# Plot function
plot_fun <- function(x){
  ggplot(data = x) +
    theme_bw() +
    geom_boxplot(mapping = aes(x = unit_type,  y = abs_RD, fill = unit_type), colour = "black") +
    labs(y = "RD", x = "") +
    scale_fill_grey() +
    scale_x_discrete(labels = c("OA","LSOA","MSOA","Ward")) + 
    theme(legend.position = "none",
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
}

# Run function through list
boxplot_list <- lapply(rd_stats_list, plot_fun)

# Save visualisations individually.
lapply(names(boxplot_list), 
       function(x)ggsave(height = 6, width = 7, filename = paste("visuals/",x,".png",sep=""), plot=boxplot_list[[x]]))

# Single visual using cowplot.
full_plot <- plot_grid(plotlist = boxplot_list, nrow = 3, ncol = 2, labels = c("(a)","(b)","(c)","(d)","(e)","(f)"), scale = 0.9)

# Save full plot.
ggsave(plot = full_plot, filename = "visuals/full_boxplot.png", height = 14, width = 8)

# Grouped plot.
rd_stats_df <- bind_rows(rd_stats_list, .id = "crime_type")

full_plot_group <- ggplot(data = rd_stats_df) +
  theme_bw() +
  geom_boxplot(mapping = aes(x = crime_type,  y = abs_RD, fill = unit_type), colour = "black", size = 0.5,
               position=position_dodge(width=1), width = 0.9) +
  labs(y = "RD", x = "", fill = "") +
  scale_fill_grey() +
  scale_x_discrete(labels = c("All crime","Residence","Theft","Vehicle","Violent")) + 
  theme(legend.position = "top",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"))

# Save full grouped plot
ggsave(plot = full_plot_group, filename = "visuals/full_boxplot_grouped.png", height = 10, width = 12)

# Function to calculate ARB stats for each crime type.
ARB_fun <- function(x){
  x %>%
    mutate(RB  = ((all_crimes / known)-1) * 100) %>% 
    group_by(unit_type) %>% 
    summarise(mean_RB    = mean(RB),
              min_RB     = min(RB),
              max_RB     = max(RB),
              var_RB     = var(RB),
              sd_RB      = sd(RB))
}

# Print stats.
lapply(rd_stats_list, ARB_fun)

# Load crimes known to Greater Manchester Police in 2011/12.
GMP_all <- read_csv(here("data/GMP_all.csv"))

# Merge spatial information of wards.
GMP_manc <- GMP_all %>%
  left_join(LSOA_to_ward, by = c("lsoa11cd" = "LSOA")) %>%
  filter(ladnm == "Manchester")

# Check to remove crimes which did not occurr within a Manchester LSOA.

# Recode crime types to match categories used thus far.
GMP_manc <- GMP_manc %>%
  mutate(Crime.type = recode(Crime.type, 
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
                             "Violent crime"               = "violent")) %>%
  filter(Crime.type == "residence" | Crime.type == "theft" | Crime.type == "vehicle" | Crime.type == "violent")

# Subset GMP_all by the different crime type categories.
GMP_all_list <- group_split(GMP_manc, Crime.type)

# Append total crimes to the list.
GMP_all_list <- c((GMP_all_list), list(GMP_manc))

# Rename elements of the list (order checked manually).
names(GMP_all_list) <- c("residence", "theft", "vehicle", "violent", "all_crimes")

# Create sf point objects for each df in the list.
GMP_all_list_sf <- lapply(GMP_all_list, function(x)st_as_sf(x, coords = c(x = "Longitude", y = "Latitude"), crs = 4326))

# Transform each sf object in the list to BNG.
GMP_all_list_sf <- lapply(GMP_all_list_sf, function(x)st_transform(x, 27700))

# Load in spatial polygons at OA, LSOA, MSOA and Ward level.
ward_sf <- st_read("data/shapefiles/ward_manc_sf.shp")
msoa_sf <- st_read("data/shapefiles/msoa_manc_sf.shp")
lsoa_sf <- st_read("data/shapefiles/lsoa_manc_sf.shp")
oa_sf   <- st_read("data/shapefiles/oa_manc_sf.shp")

# Compile into list and name elements.
sf_list <- list(ward_sf, msoa_sf, lsoa_sf, oa_sf)
names(sf_list) <- c("ward_sf","msoa_sf","lsoa_sf","oa_sf")

# Set CRS.
sf_list <- lapply(sf_list, function(x)st_transform(x, 27700))

# We now want to counts points in polygons for each combinations of crime type points (GMP_manc_sf).
# and spatial scale polygons (sf_list).

# For now, split sf_list back into different objects.
list2env(sf_list,envir=.GlobalEnv)

# Aggregating point to polygons: Ward
ward_agg_list <- lapply(GMP_all_list_sf, function(x)ward_sf %>% mutate(count = lengths(st_intersects(ward_sf, x))))

# Aggregating point to polygons: MSOA
msoa_agg_list <- lapply(GMP_all_list_sf, function(x)msoa_sf %>% mutate(count = lengths(st_intersects(msoa_sf, x))))

# Aggregating point to polygons: LSOA
lsoa_agg_list <- lapply(GMP_all_list_sf, function(x)lsoa_sf %>% mutate(count = lengths(st_intersects(lsoa_sf, x))))

# Aggregating point to polygons: OA
oa_agg_list <- lapply(GMP_all_list_sf, function(x)oa_sf %>% mutate(count = lengths(st_intersects(oa_sf, x))))

# Remove 'other' category so the merge works. We don't use it for analysis but it was needed to get the total crime figure.
#oa_agg_list   <- oa_agg_list  [-1]
#lsoa_agg_list <- lsoa_agg_list[-1]
#msoa_agg_list <- msoa_agg_list[-1]
#ward_agg_list <- ward_agg_list[-1]

# Join this real police recorded crime data with our CSEW estimates.

# Split existing unit_list into list of lists.
unit_list_crime_types <- lapply(units_list, function(x)group_split(x, unit_type))

# The first element of each list is LAD, which we don't need for the comaprison. Keeping it would complicate the join.
unit_list_crime_types <- lapply(unit_list_crime_types, function(x) x[-1])

# Collate each list into global environment by crime type. First check what objects will be called.
names(unit_list_crime_types)
list2env(unit_list_crime_types,envir=.GlobalEnv)

# Extract each spatial scale (i.e. each element of 'unit_list_crime_types').
lsoa_gmp_df <- lapply(unit_list_crime_types, function(x) x[[1]])
msoa_gmp_df <- lapply(unit_list_crime_types, function(x) x[[2]])
oa_gmp_df   <- lapply(unit_list_crime_types, function(x) x[[3]])
ward_gmp_df <- lapply(unit_list_crime_types, function(x) x[[4]])

# Order elements of each list to we can use map2 easily to for joins.
oa_gmp_df   <- oa_gmp_df  [order(names(oa_gmp_df  ))]
lsoa_gmp_df <- lsoa_gmp_df[order(names(lsoa_gmp_df))]
msoa_gmp_df <- msoa_gmp_df[order(names(msoa_gmp_df))]
ward_gmp_df <- ward_gmp_df[order(names(ward_gmp_df))]

oa_agg_list   <- oa_agg_list  [order(names(oa_agg_list  ))]
lsoa_agg_list <- lsoa_agg_list[order(names(lsoa_agg_list))]
msoa_agg_list <- msoa_agg_list[order(names(msoa_agg_list))]
ward_agg_list <- ward_agg_list[order(names(ward_agg_list))]

# Rename code name in Ward for join.
ward_agg_list <- lapply(ward_agg_list, function(x) x %>% rename(code = wd18cd))

# Join all.
join_fun <- function(x, y){
  right_join(x, y, by = c("unit" = "code"))
}

# Run through join on each list.
oa_compare_list   <- map2(oa_gmp_df  , oa_agg_list  , join_fun)
lsoa_compare_list <- map2(lsoa_gmp_df, lsoa_agg_list, join_fun)
msoa_compare_list <- map2(msoa_gmp_df, msoa_agg_list, join_fun)
ward_compare_list <- map2(ward_gmp_df, ward_agg_list, join_fun)

# Bind together for each descriptive stats. Note warning about geometry: not a problem for now, it just removes the 
# spatial attributes but retains the geometry column.
oa_compare_df   <- bind_rows(oa_compare_list  , .id = "crime_type")
lsoa_compare_df <- bind_rows(lsoa_compare_list, .id = "crime_type")
msoa_compare_df <- bind_rows(msoa_compare_list, .id = "crime_type")
ward_compare_df <- bind_rows(ward_compare_list, .id = "crime_type")


# Aggregate crime data without searching points in polygons.
# At the Output Area level.
oa_agg <- GMP_manc %>%
  group_by(Crime.type, oa11) %>%
  summarise(GMP_agg = n()) %>%
  filter(Crime.type != "other") %>%
  rename(crime_type = Crime.type,
         unit       = oa11)

oa_agg_all <- GMP_manc %>%
  group_by(oa11) %>%
  summarise(GMP_agg = n()) %>%
  mutate(Crime.type = "all_crimes") %>%
  select(Crime.type, oa11, GMP_agg) %>%
  rename(crime_type = Crime.type,
         unit       = oa11)

oa_agg <- bind_rows(oa_agg, oa_agg_all)

# At the LSOA level.
lsoa_agg <- GMP_manc %>%
  group_by(Crime.type, lsoa11cd) %>%
  summarise(GMP_agg = n()) %>%
  rename(crime_type = Crime.type,
         unit       = lsoa11cd)

lsoa_agg_all <- GMP_manc %>%
  group_by(lsoa11cd) %>%
  summarise(GMP_agg = n()) %>%
  mutate(Crime.type = "all_crimes") %>%
  select(Crime.type, lsoa11cd, GMP_agg) %>%
  rename(crime_type = Crime.type,
         unit       = lsoa11cd)

lsoa_agg <- bind_rows(lsoa_agg, lsoa_agg_all)

# At the MSOA level.
msoa_agg <- GMP_manc %>%
  group_by(Crime.type, msoa11cd) %>%
  summarise(GMP_agg = n()) %>%
  rename(crime_type = Crime.type,
         unit       = msoa11cd)

msoa_agg_all <- GMP_manc %>%
  group_by(msoa11cd) %>%
  summarise(GMP_agg = n()) %>%
  mutate(Crime.type = "all_crimes") %>%
  select(Crime.type, msoa11cd, GMP_agg) %>%
  rename(crime_type = Crime.type,
         unit       = msoa11cd)

msoa_agg <- bind_rows(msoa_agg, msoa_agg_all)

# At the Ward level.
wd_agg <- GMP_manc %>%
  group_by(Crime.type, WD18CD) %>%
  summarise(GMP_agg = n()) %>%
  rename(crime_type = Crime.type,
         unit       = WD18CD)

wd_agg_all <- GMP_manc %>%
  group_by(WD18CD) %>%
  summarise(GMP_agg = n()) %>%
  mutate(Crime.type = "all_crimes") %>%
  select(Crime.type, WD18CD, GMP_agg) %>%
  rename(crime_type = Crime.type,
         unit       = WD18CD)

wd_agg <- bind_rows(wd_agg, wd_agg_all)

# Bind togethe with main comparison dataset.
oa_compare_df <- oa_compare_df %>%
  select(-unit_type) %>%
  full_join(oa_agg, by = c("crime_type", "unit")) %>%
  replace(is.na(.), 0)
lsoa_compare_df <- lsoa_compare_df %>%
  full_join(lsoa_agg, by = c("crime_type", "unit")) %>%
  replace(is.na(.), 0)
msoa_compare_df <- msoa_compare_df %>%
  full_join(msoa_agg, by = c("crime_type", "unit")) %>%
  replace(is.na(.), 0)
ward_compare_df <- ward_compare_df %>%
  full_join(wd_agg, by = c("crime_type", "unit"))

# Map RD across all scales 19.05.2020 David
oa_levels_df <- oa_compare_df %>%
  mutate(RD = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD)) %>%
  filter(crime_type == "theft")

lsoa_levels_df <- lsoa_compare_df %>%
  mutate(RD = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD)) %>%
  filter(crime_type == "theft")

msoa_levels_df <- msoa_compare_df %>%
  mutate(RD = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD)) %>%
  filter(crime_type == "theft")

ward_levels_df <- ward_compare_df %>%
  mutate(RD = ((known - all_crimes) / all_crimes) * 100,
         abs_RD = abs(RD)) %>%
  filter(crime_type == "theft")

# Check missings
sum(is.na(oa_levels_df$abs_RD)) # This is an OA that that zero crimes (real and simulated).
sum(is.na(lsoa_levels_df$abs_RD))
sum(is.na(msoa_levels_df$abs_RD))
sum(is.na(ward_levels_df$abs_RD))

# Calculate breaks (note this omits the missing at OA-level).
oa_brks <- classIntervals(oa_levels_df$abs_RD, n = 5, style="fixed",
                          fixedBreaks = c(0, 20, 40, 60, 80, 100))

lsoa_brks <- classIntervals(lsoa_levels_df$abs_RD, n = 5, style="fixed",
                          fixedBreaks = c(0, 20, 40, 60, 80, 100))

msoa_brks <- classIntervals(msoa_levels_df$abs_RD, n = 5, style="fixed",
                            fixedBreaks = c(0, 20, 40, 60, 80, 100))

ward_brks <- classIntervals(ward_levels_df$abs_RD, n = 5, style="fixed",
                            fixedBreaks = c(0, 20, 40, 60, 80, 100))

oa_levels_sf <- oa_levels_df %>% 
  drop_na(abs_RD)  %>% # Drop the observation which had no crimes and thus no abs_rd  
  st_as_sf(sf_column_name = "geometry") %>% 
  mutate(RD_cut = cut(abs_RD, oa_brks$brks, include.lowest = T, dig.lab = 5)) %>%
  mutate(RD_cut = fct_recode(RD_cut, "0-20" = "[0,20]", "20-40" = "(20,40]", "40-60" = "(40,60]", "60-80" = "(60,80]", "80-100"="(80,100]"))

lsoa_levels_sf <- lsoa_levels_df %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  mutate(RD_cut = cut(abs_RD, lsoa_brks$brks, include.lowest = T, dig.lab = 5)) %>% 
  mutate(RD_cut = fct_recode(RD_cut, "0-20" = "[0,20]", "20-40" = "(20,40]", "40-60" = "(40,60]", "60-80" = "(60,80]", "80-100"="(80,100]"))
table(lsoa_levels_sf$RD_cut)

msoa_levels_sf <- msoa_levels_df %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  mutate(RD_cut = cut(abs_RD, msoa_brks$brks, include.lowest = T, dig.lab = 5)) %>% 
  mutate(RD_cut = fct_recode(RD_cut, "0-20" = "[0,20]", "20-40" = "(20,40]", "40-60" = "(40,60]", "60-80" = "(60,80]", "80-100"="(80,100]"))
table(msoa_levels_sf$RD_cut)

ward_levels_sf <- ward_levels_df %>% 
  st_as_sf(sf_column_name = "geometry") %>% 
  mutate(RD_cut = cut(abs_RD, ward_brks$brks, include.lowest = T, dig.lab = 5)) %>% 
  mutate(RD_cut = fct_recode(RD_cut, "0-20" = "[0,20]", "20-40" = "(20,40]", "40-60" = "(40,60]", "60-80" = "(60,80]", "80-100"="(80,100]"))
table(ward_levels_sf$RD_cut)


# Get 5 set colours, avoiding the overly light one, so ask for 6 and get 5.
greypal <- brewer_pal(palette = "Greys")(6)[2:6] # main
# "#D9D9D9", "#BDBDBD", "#969696", "#636363", "#252525"

# OA plot.
p1 <- ggplot(data = oa_levels_sf) +
    geom_sf(mapping = aes(fill = RD_cut), colour = "transparent") +
    theme_void() +
    scale_fill_manual(values = greypal) +
    labs(fill = "RD %", title = "OA") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())

# LSOA plot.
p2 <- ggplot(data = lsoa_levels_sf) +
    geom_sf(mapping = aes(fill = RD_cut), colour = "transparent") +
    theme_void() +
    scale_fill_manual(values = c("#969696", "#636363")) +
    labs(fill = "RD %", title = "LSOA") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())

# MSOA plot.  
p3 <- ggplot(data = msoa_levels_sf) +
    geom_sf(mapping = aes(fill = RD_cut), colour = "transparent") +
    theme_void() +
    scale_fill_manual(values = c("#969696", "#636363")) +
    labs(fill = "RD %", title = "MSOA") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())

# Ward plot.
p4 <- ggplot(data = ward_levels_sf) +
    geom_sf(mapping = aes(fill = RD_cut), colour = "transparent") +
    theme_void() +
    scale_fill_manual(values = c("#969696", "#636363")) +
    labs(fill = "RD %", title = "Ward") +
    theme(legend.position = "none", 
          axis.text.x = element_blank(),
          axis.text.y = element_blank())

# Plot just to get legend.
leg <- ggplot(data = oa_levels_sf) + theme_minimal() + geom_sf(mapping = aes(fill = RD_cut), colour = "transparent") + scale_fill_manual(values = greypal) + labs(fill = "RD %") + theme(legend.position = "bottom")
leg_p <- get_legend(leg)


# Arrange plots.
maps_plot <- plot_grid(p1, p2, p3, p4, nrow = 1)
full_plot <- plot_grid(maps_plot, leg_p, nrow = 2)
ggsave(plot = full_plot, filename = "visuals/map_comaprison_RD.png", height = 24, width = 24, unit = "cm")

# Compute correlations between crimes known to police (simulated data) and crime recorded by GMP (direct aggregates).
cor_oa_df <- oa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, GMP_agg, method = "spearman")$estimate,
            p    = cor.test(known, GMP_agg, method = "spearman")$p.value)
cor_oa_df

cor_lsoa_df <- lsoa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, GMP_agg, method = "spearman")$estimate,
            p    = cor.test(known, GMP_agg, method = "spearman")$p.value)
cor_lsoa_df

cor_msoa_df <- msoa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, GMP_agg, method = "spearman")$estimate,
            p    = cor.test(known, GMP_agg, method = "spearman")$p.value)
cor_msoa_df

cor_ward_df <- ward_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, GMP_agg, method = "spearman")$estimate,
            p    = cor.test(known, GMP_agg, method = "spearman")$p.value)
cor_ward_df

# Compute correlations between crimes known to police (simulated data) and crime recorded by GMP (points in polygons).
cor_oa_df <- oa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, count, method = "spearman")$estimate,
            p    = cor.test(known, count, method = "spearman")$p.value)
cor_oa_df

cor_lsoa_df <- lsoa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, count, method = "spearman")$estimate,
            p    = cor.test(known, count, method = "spearman")$p.value)
cor_lsoa_df

cor_msoa_df <- msoa_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, count, method = "spearman")$estimate,
            p    = cor.test(known, count, method = "spearman")$p.value)
cor_msoa_df

cor_ward_df <- ward_compare_df %>% 
  group_by(crime_type) %>%
  summarise(corr = cor.test(known, count, method = "spearman")$estimate,
            p    = cor.test(known, count, method = "spearman")$p.value)
cor_ward_df

# Regain spatial attributes.
oa_compare_sf_list   <- lapply(oa_compare_list  , function(x) st_as_sf(x))
lsoa_compare_sf_list <- lapply(lsoa_compare_list, function(x) st_as_sf(x))
msoa_compare_sf_list <- lapply(msoa_compare_list, function(x) st_as_sf(x))
ward_compare_sf_list <- lapply(ward_compare_list, function(x) st_as_sf(x))

# Proximity matrix function to compute Local Moran's I.
# Adjust tolerance to avoid errenous 'no neighbours'. The figure 1e-005 used based on default in GeoDa 1.14.
prox_fun <- function(x) {
  temp <- poly2nb(x, row.names = x$unit, snap = 1e-005) 
  nb2listw(temp) 
}

# Run for each spatial scale (any one of the elements in the list will do).
oa_prox   <- prox_fun(oa_compare_sf_list  [[1]])
lsoa_prox <- prox_fun(lsoa_compare_sf_list[[1]])
msoa_prox <- prox_fun(msoa_compare_sf_list[[1]])
ward_prox <- prox_fun(ward_compare_sf_list[[1]])

# Check neighours stats compared to GeoDa. Results are either identical or very similar.
oa_prox$neighbours
lsoa_prox$neighbours
msoa_prox$neighbours
ward_prox$neighbours

# Global Morans'I.
mi_oa_df <- oa_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ count), oa_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ count), oa_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ count), oa_prox)$p.value)
mi_oa_df

mi_oa_df <- oa_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ GMP_agg), oa_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ GMP_agg), oa_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ GMP_agg), oa_prox)$p.value)

mi_lsoa_df <- lsoa_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ count), lsoa_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ count), lsoa_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ count), lsoa_prox)$p.value)
mi_lsoa_df

mi_msoa_df <- msoa_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ count), msoa_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ count), msoa_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ count), msoa_prox)$p.value)
mi_msoa_df

mi_msoa_df <- msoa_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ GMP_agg), msoa_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ GMP_agg), msoa_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ GMP_agg), msoa_prox)$p.value)

mi_ward_df <- ward_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ count), ward_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ count), ward_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ count), ward_prox)$p.value)
mi_ward_df

mi_ward_df <- ward_compare_df %>%
  group_by(crime_type) %>%
  summarise(mi   = lm.morantest.exact(lm(known ~ GMP_agg), ward_prox)$estimate,
            stat = lm.morantest.exact(lm(known ~ GMP_agg), ward_prox)$statistic,
            p    = lm.morantest.exact(lm(known ~ GMP_agg), ward_prox)$p.value)

# Empirical evaluation of simulated dataset from CSEW data

# Create three agre groups - CSEW data.
csew$age_rec <- NA
csew$age_rec[csew$age < 36] <- "less35"
csew$age_rec[csew$age >= 36 & csew$age < 56] <- "36to55"
csew$age_rec[csew$age >= 56] <- "56more"

# Create three age groups - simulated data.
syn_res_OA$age_rec <- NA
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
csew_vf_vehicle$age_rec <- NA
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
csew_vf_residence$age_rec <- NA
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
csew_vf_theft$age_rec <- NA
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
csew_vf_violence$age_rec <- NA
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