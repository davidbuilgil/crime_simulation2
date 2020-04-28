
###################################
#
# Simulation study levels of geography and crime data
#
# Angelo Moretti and David Buil-Gil
#
###################################

rm(list=ls())

setwd("D:/Fellowship Manchester/Papers/Paper Angelo/Data")

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

Data$No_income_mean <- Data2$mean_not_income

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
