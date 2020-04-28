
###################################
#
# Simulation study levels of geography and crime data
#
# Angelo Moretti and David Buil-Gil
#
###################################

rm(list=ls())

setwd("E:/Fellowship Manchester/Papers/Paper Angelo/Data")

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

model_vehicle <- glm(vehicle ~ age + sex + reseth + remploy + educat2, family = poisson, data = csew)
summary(model_vehicle)

model_residence <- glm(residence ~ age + sex + reseth + remploy + educat2, family = poisson, data = csew)
summary(model_residence)

model_theft_dam <- glm(theft_dam ~ age + sex + reseth + remploy + educat2, family = poisson, data = csew)
summary(model_theft_dam)

model_violence <- glm(violence ~ age + sex + reseth + remploy + educat2, family = poisson, data = csew)
summary(model_violence)

model_all_crimes <- glm(all_crimes ~ age + sex + reseth + remploy + educat2, family = poisson, data = csew)
summary(model_all_crimes)


