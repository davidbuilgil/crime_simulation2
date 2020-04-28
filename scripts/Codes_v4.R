
###################################
#
# Simulation study levels of geography and crime data
#
# Angelo Moretti and David Buil-Gil
#
###################################

rm(list=ls())

setwd("D:/Fellowship Manchester/Papers/Paper Angelo/Data")

library(readxl)

Age_by_OA_Manchester <- read_excel("Age_by_OA_Manchester.xlsx")

N <- sum(Age_by_OA_Manchester$Pop)

D <- nrow(Age_by_OA_Manchester)

Data <- as.data.frame(matrix(NA, nrow = N, ncol = 13))

colnames(Data)[1] <- "ID"

Data$ID <- 1:N

colnames(Data)[2] <- "OA"

Data$OA <- rep.int(Age_by_OA_Manchester$OA, time=Age_by_OA_Manchester$Pop)

colnames(Data)[3] <- "Age"

colnames(Data)[4] <- "mean_age_OA"

colnames(Data)[5] <- "sd_age_OA"

Data2 <- merge(Data, Age_by_OA_Manchester, by = "OA", all.x = TRUE)

Data$mean_age_OA <- Data2$Mean
Data$sd_age_OA <- Data2$SD

Data$Age <- rnorm(N, Data$mean_age_OA, Data$sd_age_OA)

Data$mean_age_OA <- NULL
Data$sd_age_OA <- NULL

colnames(Data)[4] <- "Sex"

Sex_by_OA_Manchester <- read_excel("Sex_by_OA_Manchester.xlsx")

colnames(Sex_by_OA_Manchester)[1] <- "OA"

Data2 <- merge(Data, Sex_by_OA_Manchester, by = "OA", all.x = TRUE)

colnames(Data)[5] <- "mean_male"

Data$mean_male <- Data2$Mean_male

Data$Sex <- rbinom(N, 1, Data$mean_male)

Data$mean_male <- NULL

Ethnicity_by_OA_Manchester <- read_excel("Ethnicity_by_OA_Manchester.xlsx")

colnames(Ethnicity_by_OA_Manchester)[1] <- "OA"

Data2 <- merge(Data, Ethnicity_by_OA_Manchester, by = "OA", all.x = TRUE)

colnames(Data)[4] <- "Male"
colnames(Data)[5] <- "White"
colnames(Data)[6] <- "mean_white"

Data$mean_white <- Data2$Mean_white

Data$White <- rbinom(N, 1, Data$mean_white)

Data$mean_white <- NULL

Income_by_OA_Manchester <- read_excel("Income_by_OA_Manchester.xlsx")

colnames(Income_by_OA_Manchester)[1] <- "OA"

Data2 <- merge(Data, Income_by_OA_Manchester, by = "OA", all.x = TRUE)

colnames(Data)[6] <- "No_income"
colnames(Data)[7] <- "No_income_mean"

Data$No_income_mean <- Data2$mean_not_income #Non-working population (including unemployed, students, retired, long term sick and looking after home)

Data$No_income <- rbinom(N, 1, Data$No_income_mean)

Data$No_income_mean <- NULL

Edu_by_OA_Manchester <- read_excel("Edu_by_OA_Manchester.xlsx")

colnames(Edu_by_OA_Manchester)[1] <- "OA"

Data2 <- merge(Data, Edu_by_OA_Manchester, by = "OA", all.x = TRUE)

colnames(Data)[7] <- "High_edu"
colnames(Data)[8] <- "High_edu_mean"

Data$High_edu_mean <- Data2$Mean_level4_edu

Data$High_edu <- rbinom(N, 1, Data$High_edu_mean)

Data$High_edu_mean <- NULL

library(haven)

csew <- read_sav("csew_apr11mar12_nvf.sav")

#Age, male, white, No_income, High_edu

table(csew$age)
csew$age[csew$age > 120] <- NA
summary(csew$age)
Data_more16 <- Data[ which(Data$Age >= 16), ]
summary(Data_more16$Age)

table(csew$sex)
csew$sex[csew$sex == 2] <- 0
prop.table(table(csew$sex))
prop.table(table(Data$Male))

table(csew$reseth)
csew$reseth[csew$reseth != 1] <- 0
prop.table(table(csew$reseth))
prop.table(table(Data$White))

table(csew$work2)
csew$work2[csew$work2 > 3] <- NA
csew$work2[csew$work2 == 2] <- 0
prop.table(table(csew$work2))
prop.table(table(Data$No_income))

table(csew$remploy)
csew$remploy[csew$remploy == 1] <- 0
csew$remploy[csew$remploy == 2 | csew$remploy == 3] <- 1
prop.table(table(csew$remploy))
prop.table(table(Data$No_income))

table(csew$educat2)
csew$educat2[csew$educat2 > 10] <- NA
csew$educat2[csew$educat2 == 1 | csew$educat2 == 2 | csew$educat2 == 3] <- 1
csew$educat2[csew$educat2 == 4 | csew$educat2 == 5 | csew$educat2 == 6 | csew$educat2 == 7 | csew$educat2 == 8] <- 0
prop.table(table(csew$educat2))
prop.table(table(Data$High_edu))

#vandalism or theft of or from vehicle: nmotthef (vehicle stolen), nmotstol (something stolen from vehicle), ncardam (vehicle damaged by vandals),
  #nbikthet (theft of bike)
#vandalism or theft from residence: nprevthe (someont get in and steal something), nprevdam (get into house and cause damage), 
  #nprevtry (try to fet in house without permission), nprevsto (something stolen out of house), nproside (anything stolen from doorstep, garden, garage),
  #nprdefac (something damaged outside house), nhomthef (someone get into new house and steal), nyrhthef (steal from new house), nyrhodam (damage new house),
  #nyrhotry (tried to get in new house), nyrhosto (stolen out from new house), nyroside (stolen from garden new house), nyrdefac (damage outside new house)
#theft: npersth (something you were carrying stolen out of hands), ntrypers (tried to steal out from hands), noththef (something stolen)
#damage: ndelibda
#violence: ndelibv (hit you with fits or weapon), nthrevio (threat to use violence), nsexatt (sexual assault or interference), nhhldvio (violence from
  #someone in household)

table(csew$nmotthef)
csew$nmotthef[is.na(csew$nmotthef)] <- 0

table(csew$nmotstol)
csew$nmotstol[is.na(csew$nmotstol)] <- 0

table(csew$ncardam)
csew$ncardam[is.na(csew$ncardam)] <- 0

table(csew$nbikthef)
csew$nbikthef[is.na(csew$nbikthef)] <- 0

csew$vehicle <- csew$nmotthef + csew$nmotstol + csew$ncardam + csew$nbikthef
table(csew$vehicle)
plot(table(csew$vehicle))

table(csew$nprevthe)
csew$nprevthe[is.na(csew$nprevthe)] <- 0

table(csew$nprevdam)
csew$nprevdam[is.na(csew$nprevdam)] <- 0

table(csew$nprevtry)
csew$nprevtry[is.na(csew$nprevtry)] <- 0

table(csew$nprevsto)
csew$nprevsto[is.na(csew$nprevsto)] <- 0

table(csew$nproside)
csew$nproside[is.na(csew$nproside)] <- 0

table(csew$nprdefac)
csew$nprdefac[is.na(csew$nprdefac)] <- 0

table(csew$nhomthef)
csew$nhomthef[is.na(csew$nhomthef)] <- 0

table(csew$nyrhthef)
csew$nyrhthef[is.na(csew$nyrhthef)] <- 0

table(csew$nyrhodam)
csew$nyrhodam[is.na(csew$nyrhodam)] <- 0

table(csew$nyrhotry)
csew$nyrhotry[is.na(csew$nyrhotry)] <- 0

table(csew$nyrhosto)
csew$nyrhosto[is.na(csew$nyrhosto)] <- 0

table(csew$nyroside)
csew$nyroside[is.na(csew$nyroside)] <- 0

table(csew$nyrdefac)
csew$nyrdefac[is.na(csew$nyrdefac)] <- 0

csew$residence <- csew$nprevthe + csew$nprevdam + csew$nprevtry + csew$nprevsto + csew$nproside + csew$nprdefac + csew$nhomthef + csew$nyrhthef +
  csew$nyrhodam + csew$nyrhotry + csew$nyrhosto + csew$nyroside + csew$nyrdefac
table(csew$residence)
plot(table(csew$residence))

table(csew$npersth)
csew$npersth[is.na(csew$npersth)] <- 0

table(csew$ntrypers)
csew$ntrypers[is.na(csew$ntrypers)] <- 0

table(csew$noththef)
csew$noththef[is.na(csew$noththef)] <- 0

csew$theft <- csew$npersth + csew$ntrypers + csew$noththef
table(csew$theft)
plot(table(csew$theft))

table(csew$ndelibda)
csew$ndelibda[is.na(csew$ndelibda)] <- 0
plot(table(csew$ndelibda))

csew$theft_dam <- csew$theft + csew$ndelibda
plot(table(csew$theft_dam)) 

table(csew$ndelibv)
csew$ndelibv[is.na(csew$ndelibv)] <- 0

table(csew$nthrevio)
csew$nthrevio[is.na(csew$nthrevio)] <- 0

table(csew$nsexatt)
csew$nsexatt[is.na(csew$nsexatt)] <- 0

table(csew$nhhldvio)
csew$nhhldvio[is.na(csew$nhhldvio)] <- 0

csew$violence <- csew$ndelibv + csew$nthrevio + csew$nsexatt + csew$nhhldvio
table(csew$violence)
plot(table(csew$violence))

csew$all_crimes <- csew$vehicle + csew$residence + csew$theft + csew$ndelibda + csew$violence
table(csew$all_crimes)
plot(table(csew$all_crimes))

table(csew$victim)

library(MASS)

model_vehicle <- glm.nb(vehicle ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_vehicle)

model_residence <- glm.nb(residence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_residence)

model_theft <- glm.nb(theft ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_theft)

model_violence <- glm.nb(violence ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_violence)

model_all_crimes <- glm.nb(all_crimes ~ age + sex + reseth + remploy + educat2, data = csew)
summary(model_all_crimes)

library(stats)

eta_vehicle <- model_vehicle$coefficients[1] + Data$Age * model_vehicle$coefficients[2] +
  Data$Male * model_vehicle$coefficients[3] + Data$White * model_vehicle$coefficients[4] +
  Data$No_income * model_vehicle$coefficients[5] + Data$High_edu * model_vehicle$coefficients[6]

Data$vehicle <- rnbinom(length(eta_vehicle), mu = exp(eta_vehicle), size = model_vehicle$theta)
plot(table(csew$vehicle))
plot(table(Data$vehicle))

eta_residence <- model_residence$coefficients[1] + Data$Age * model_residence$coefficients[2] +
  Data$Male * model_residence$coefficients[3] + Data$White * model_residence$coefficients[4] +
  Data$No_income * model_residence$coefficients[5] + Data$High_edu * model_residence$coefficients[6]

Data$residence <- rnbinom(length(eta_residence), mu = exp(eta_residence), size = model_residence$theta)
plot(table(csew$residence))
plot(table(Data$residence))

eta_theft <- model_theft$coefficients[1] + Data$Age * model_theft$coefficients[2] +
  Data$Male * model_theft$coefficients[3] + Data$White * model_theft$coefficients[4] +
  Data$No_income * model_theft$coefficients[5] + Data$High_edu * model_theft$coefficients[6]

Data$theft <- rnbinom(length(eta_theft), mu = exp(eta_theft), size = model_theft$theta)
plot(table(csew$theft))
plot(table(Data$theft))

eta_violence <- model_violence$coefficients[1] + Data$Age * model_violence$coefficients[2] +
  Data$Male * model_violence$coefficients[3] + Data$White * model_violence$coefficients[4] +
  Data$No_income * model_violence$coefficients[5] + Data$High_edu * model_violence$coefficients[6]

Data$violence <- rnbinom(length(eta_violence), mu = exp(eta_violence), size = model_violence$theta)
plot(table(csew$violence))
plot(table(Data$violence))

csew_vf <- read_sav("csew_apr11mar12_vf.sav")

Data_vehicle <- Data

Data_vehicle <- Data_vehicle[ which(Data_vehicle$vehicle != 0), ]

Data_vehicle <- as.data.frame(lapply(Data_vehicle, rep, Data_vehicle$vehicle))

Data_vehicle$vehicle[Data_vehicle$vehicle > 0] <- 1
Data_vehicle$residence <- 0
Data_vehicle$theft <- 0
Data_vehicle$violence <- 0

Data_residence <- Data

Data_residence <- Data_residence[ which(Data_residence$residence != 0), ]

Data_residence <- as.data.frame(lapply(Data_residence, rep, Data_residence$residence))

Data_residence$residence[Data_residence$residence > 0] <- 1
Data_residence$vehicle <- 0
Data_residence$theft <- 0
Data_residence$violence <- 0

Data_theft <- Data

Data_theft <- Data_theft[ which(Data_theft$theft != 0), ]

Data_theft <- as.data.frame(lapply(Data_theft, rep, Data_theft$theft))

Data_theft$theft[Data_theft$theft > 0] <- 1
Data_theft$vehicle <- 0
Data_theft$residence <- 0
Data_theft$violence <- 0

Data_violence <- Data

Data_violence <- Data_violence[ which(Data_violence$violence != 0), ]

Data_violence <- as.data.frame(lapply(Data_violence, rep, Data_violence$violence))

Data_violence$violence[Data_violence$violence > 0] <- 1
Data_violence$vehicle <- 0
Data_violence$residence <- 0
Data_violence$theft <- 0

csew_vf <- merge(csew_vf, csew, by = "rowlabel", all.x = TRUE)

csew_vf$copsknow[csew_vf$copsknow == 2] <- 0
csew_vf$copsknow[csew_vf$copsknow == 9] <- NA
csew_vf$copsknow[csew_vf$copsknow == 8] <- NA
prop.table(table(csew_vf$copsknow))

csew_vf_vehicle <- csew_vf[ which(csew_vf$crimtype == 1 | csew_vf$crimtype == 2 | csew_vf$crimtype == 3 |
                                    csew_vf$crimtype == 4), ]

csew_vf_residence <- csew_vf[ which(csew_vf$crimtype == 5 | csew_vf$crimtype == 6 | csew_vf$crimtype == 7 |
                                    csew_vf$crimtype == 8 | csew_vf$crimtype == 9 | csew_vf$crimtype == 10 |
                                    csew_vf$crimtype == 11 | csew_vf$crimtype == 12 | csew_vf$crimtype == 13 |
                                    csew_vf$crimtype == 14 | csew_vf$crimtype == 15 | csew_vf$crimtype == 16 |
                                    csew_vf$crimtype == 17), ]

csew_vf_theft <- csew_vf[ which(csew_vf$crimtype == 18 | csew_vf$crimtype == 19 | csew_vf$crimtype == 20), ]

csew_vf_violence <- csew_vf[ which(csew_vf$crimtype == 22 | csew_vf$crimtype == 23 | csew_vf$crimtype == 24 |
                                     csew_vf$crimtype == 25), ]

model_repo_vehicle <- glm(copsknow ~ age + sex + reseth + remploy + educat2, family = binomial(link = "logit"),
                          data = csew_vf_vehicle)
summary(model_repo_vehicle)

model_repo_residence <- glm(copsknow ~ age + sex + reseth + remploy + educat2, family = binomial(link = "logit"),
                          data = csew_vf_residence)
summary(model_repo_residence)

model_repo_theft <- glm(copsknow ~ age + sex + reseth + remploy + educat2, family = binomial(link = "logit"),
                            data = csew_vf_theft)
summary(model_repo_theft)

model_repo_violence <- glm(copsknow ~ age + sex + reseth + remploy + educat2, family = binomial(link = "logit"),
                        data = csew_vf_violence)
summary(model_repo_violence)

Data_vehicle$copsknow <- model_repo_vehicle$coefficients[1] + Data_vehicle$Age * model_repo_vehicle$coefficients[2] +
  Data_vehicle$Male * model_repo_vehicle$coefficients[3] + Data_vehicle$White * model_repo_vehicle$coefficients[4] +
  Data_vehicle$No_income * model_repo_vehicle$coefficients[5] + Data_vehicle$High_edu * model_repo_vehicle$coefficients[6]

Data_vehicle$copsknow <- exp(Data_vehicle$copsknow) / (1 + exp(Data_vehicle$copsknow))

Data_vehicle$copsknow <- rbinom(nrow(Data_vehicle), 1, Data_vehicle$copsknow)

plot(table(csew_vf_vehicle$copsknow))
plot(table(Data_vehicle$copsknow))

Data_residence$copsknow <- model_repo_residence$coefficients[1] + Data_residence$Age * model_repo_residence$coefficients[2] +
  Data_residence$Male * model_repo_residence$coefficients[3] + Data_residence$White * model_repo_residence$coefficients[4] +
  Data_residence$No_income * model_repo_residence$coefficients[5] + Data_residence$High_edu * model_repo_residence$coefficients[6]

Data_residence$copsknow <- exp(Data_residence$copsknow) / (1 + exp(Data_residence$copsknow))

Data_residence$copsknow <- rbinom(nrow(Data_residence), 1, Data_residence$copsknow)

plot(table(csew_vf_residence$copsknow))
plot(table(Data_residence$copsknow))

Data_theft$copsknow <- model_repo_theft$coefficients[1] + Data_theft$Age * model_repo_theft$coefficients[2] +
  Data_theft$Male * model_repo_theft$coefficients[3] + Data_theft$White * model_repo_theft$coefficients[4] +
  Data_theft$No_income * model_repo_theft$coefficients[5] + Data_theft$High_edu * model_repo_theft$coefficients[6]

Data_theft$copsknow <- exp(Data_theft$copsknow) / (1 + exp(Data_theft$copsknow))

Data_theft$copsknow <- rbinom(nrow(Data_theft), 1, Data_theft$copsknow)

plot(table(csew_vf_theft$copsknow))
plot(table(Data_theft$copsknow))

Data_violence$copsknow <- model_repo_violence$coefficients[1] + Data_violence$Age * model_repo_violence$coefficients[2] +
  Data_violence$Male * model_repo_violence$coefficients[3] + Data_violence$White * model_repo_violence$coefficients[4] +
  Data_violence$No_income * model_repo_violence$coefficients[5] + Data_violence$High_edu * model_repo_violence$coefficients[6]

Data_violence$copsknow <- exp(Data_violence$copsknow) / (1 + exp(Data_violence$copsknow))

Data_violence$copsknow <- rbinom(nrow(Data_violence), 1, Data_violence$copsknow)

plot(table(csew_vf_violence$copsknow))
plot(table(Data_violence$copsknow))

Data_crimes <- rbind(Data_vehicle, Data_residence, Data_theft, Data_violence)

OA_to_LAD <- read.csv("D:/Fellowship Manchester/Papers/Paper Angelo/Data/Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_December_2017_Lookup_in_Great_Britain__Classification_Version_2.csv")
colnames(OA_to_LAD)[1] <- "OA"
OA_to_LAD <- OA_to_LAD[ which(OA_to_LAD$LAD17NM == "Manchester"), ]

Data_crimes2 <- merge(Data_crimes, OA_to_LAD, by = "OA", all.x = TRUE)
Data_crimes2$OAC11CD <- NULL
Data_crimes2$OAC11NM <- NULL
colnames(Data_crimes2)[13] <- "LSOA"
Data_crimes2$LSOA11NM <- NULL
Data_crimes2$SOAC11CD <- NULL
Data_crimes2$SOAC11NM <- NULL
Data_crimes2$MSOA11NM <- NULL
Data_crimes2$FID <- NULL
Data_crimes2$CTRY11NM <- NULL
Data_crimes2$CTRY11CD <- NULL
Data_crimes2$RGN11NM <- NULL
Data_crimes2$RGN11CD <- NULL
Data_crimes2$LACNM <- NULL
Data_crimes2$LACCD <- NULL
Data_crimes2$LAD17NM <- NULL
colnames(Data_crimes2)[14] <- "MSOA"
colnames(Data_crimes2)[15] <- "LAD"

Data_crimes <- Data_crimes2

LSOA_to_ward <- read.csv("D:/Fellowship Manchester/Papers/Paper Angelo/Data/Lower_Layer_Super_Output_Area_2011_to_Ward_2018_Lookup_in_England_and_Wales_v3.csv")
colnames(LSOA_to_ward)[1] <- "LSOA"
LSOA_to_ward <- LSOA_to_ward[ which(LSOA_to_ward$LAD18NM == "Manchester"), ]

OA_to_LAD <- OA_to_LAD[ which(OA_to_LAD$LAD17NM == "Manchester"), ]

Data_crimes2 <- merge(Data_crimes, LSOA_to_ward, by = "LSOA", all.x = TRUE)

Data_crimes2$FID <- NULL
Data_crimes2$LAD18NM <- NULL
Data_crimes2$LAD18CD <- NULL
Data_crimes2$WD18NMW <- NULL
Data_crimes2$WD18NM <- NULL
Data_crimes2$LSOA11NM <- NULL
colnames(Data_crimes2)[16] <- "WD"

Data_crimes <- Data_crimes2

write.csv(Data_crimes, "Data_crimes.csv")

rm(list=ls())

Data_crimes <- read.csv("D:/Fellowship Manchester/Papers/Paper Angelo/Data/Data_crimes.csv")

mean(Data_crimes$copsknow)

length(table(Data_crimes$OA))
length(table(Data_crimes$LSOA))
length(table(Data_crimes$MSOA))
length(table(Data_crimes$WD))
length(table(Data_crimes$LAD))

Data_crimes$ind <- 1

crimes_OA <- aggregate(Data_crimes$ind, list(Data_crimes$OA), sum)
colnames(crimes_OA)[2] <- "all_crimes"
crimes_OA_know <- aggregate(Data_crimes$copsknow, list(Data_crimes$OA), sum)
colnames(crimes_OA_know)[2] <- "known"
crimes_OA <- merge(crimes_OA, crimes_OA_know, by = "Group.1", all.x = TRUE)

crimes_LSOA <- aggregate(Data_crimes$ind, list(Data_crimes$LSOA), sum)
colnames(crimes_LSOA)[2] <- "all_crimes"
crimes_LSOA_know <- aggregate(Data_crimes$copsknow, list(Data_crimes$LSOA), sum)
colnames(crimes_LSOA_know)[2] <- "known"
crimes_LSOA <- merge(crimes_LSOA, crimes_LSOA_know, by = "Group.1", all.x = TRUE)

crimes_MSOA <- aggregate(Data_crimes$ind, list(Data_crimes$MSOA), sum)
colnames(crimes_MSOA)[2] <- "all_crimes"
crimes_MSOA_know <- aggregate(Data_crimes$copsknow, list(Data_crimes$MSOA), sum)
colnames(crimes_MSOA_know)[2] <- "known"
crimes_MSOA <- merge(crimes_MSOA, crimes_MSOA_know, by = "Group.1", all.x = TRUE)

crimes_WD <- aggregate(Data_crimes$ind, list(Data_crimes$WD), sum)
colnames(crimes_WD)[2] <- "all_crimes"
crimes_WD_know <- aggregate(Data_crimes$copsknow, list(Data_crimes$WD), sum)
colnames(crimes_WD_know)[2] <- "known"
crimes_WD <- merge(crimes_WD, crimes_WD_know, by = "Group.1", all.x = TRUE)

crimes_LAD <- aggregate(Data_crimes$ind, list(Data_crimes$LAD), sum)
colnames(crimes_LAD)[2] <- "all_crimes"
crimes_LAD_know <- aggregate(Data_crimes$copsknow, list(Data_crimes$LAD), sum)
colnames(crimes_LAD_know)[2] <- "known"
crimes_LAD <- merge(crimes_LAD, crimes_LAD_know, by = "Group.1", all.x = TRUE)

crimes_OA$RD <- ((crimes_OA$known - crimes_OA$all_crimes) / crimes_OA$all_crimes) * 100
crimes_OA$Abs_RD <- abs(crimes_OA$RD)
var(crimes_OA$RD)
sd(crimes_OA$RD)
median(crimes_OA$Abs_RD)
summary(crimes_OA$Abs_RD)
median(crimes_OA$RD)

crimes_LSOA$RD <- ((crimes_LSOA$known - crimes_LSOA$all_crimes) / crimes_LSOA$all_crimes) * 100
crimes_LSOA$Abs_RD <- abs(crimes_LSOA$RD)
var(crimes_LSOA$RD)
sd(crimes_LSOA$RD)
median(crimes_LSOA$Abs_RD)
summary(crimes_LSOA$Abs_RD)
median(crimes_LSOA$RD)

crimes_MSOA$RD <- ((crimes_MSOA$known - crimes_MSOA$all_crimes) / crimes_MSOA$all_crimes) * 100
crimes_MSOA$Abs_RD <- abs(crimes_MSOA$RD)
var(crimes_MSOA$RD)
sd(crimes_MSOA$RD)
median(crimes_MSOA$Abs_RD)
summary(crimes_MSOA$Abs_RD)
median(crimes_MSOA$RD)

crimes_WD$RD <- ((crimes_WD$known - crimes_WD$all_crimes) / crimes_WD$all_crimes) * 100
crimes_WD$Abs_RD <- abs(crimes_WD$RD)
var(crimes_WD$RD)
sd(crimes_WD$RD)
median(crimes_WD$Abs_RD)
summary(crimes_WD$Abs_RD)
median(crimes_WD$RD)

crimes_LAD$RD <- ((crimes_LAD$known - crimes_LAD$all_crimes) / crimes_LAD$all_crimes) * 100
crimes_LAD$Abs_RD <- abs(crimes_LAD$RD)
median(crimes_LAD$Abs_RD)
median(crimes_LAD$RD)

boxplot(crimes_OA$Abs_RD, crimes_LSOA$Abs_RD, crimes_MSOA$Abs_RD, crimes_WD$Abs_RD, col = "gold")
title("Boxplots of RD% Between All Crimes and Crimes Known to Police")

library(vioplot)

vioplot(crimes_OA$Abs_RD, crimes_LSOA$Abs_RD, crimes_MSOA$Abs_RD, crimes_WD$Abs_RD, 
        names=c("OAs", "LSOAs", "MSOAs", "Wards"), ylim = c(0, 100),
        col="gold")
title("Violin Plots of RD% Between All Crimes and Crimes Known to Police")


