## Smallholder households' farming knowledge affects soil ability in marginal areas
## Occelli, Mantino, Ragaglini, Dell'Acqua, Pè and Nuvolari

# Script for the implementation of the knowledge - soil ability analysis

# Load libraries
library(VGAM)
library(readxl)
library(MASS)
library(ggplot2)
library(tidyverse)

# Set your working directory

# Load dataset
data <- read_excel("Ability_knowledge_master.xlsx", 
                                        col_types = c("numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", "numeric"))

data <- na.omit(data)
View(data)

# Create sub-dataset for Amhara and Tigray
data_amhara <- subset(data, hhid<5500000)
data_tigray <- subset(data, hhid>5500000)
View(data_tigray)
View(data_amhara)

# Clean dataset
data <- subset(data[-c(1)])
data_amhara <- subset(data_amhara[-c(1)])
data_tigray <- subset(data_tigray[-c(1)])

# Regressing the household's farming knowledge on soil ability
# Firstly, all regions together
# Generalized Poisson model for under-dispersed data
 
# Model1 (knowledge factors only)
reg_all <- vglm(q_tot_npk ~ k_home + k_social + hh_education, genpoisson, data = data)
summary(reg_all)
AICc(reg_all)

#Model2 (resource level controls inserted)
reg_all_2 <- vglm(q_tot_npk ~ k_home + k_social + hh_education + offfarm_income + euc , genpoisson, data = data)
summary(reg_all_2)
AICc(reg_all_2)

#Model3 (resource and farmer level controls inserted)
reg_all_3 <- vglm(q_tot_npk ~ k_home + k_social + hh_education + offfarm_income + euc + hh_age + hh_gender, genpoisson, data = data)
summary(reg_all_3)
AICc(reg_all_3)

# Computation for sub-region: Amhara 

#Model1
reg_amhara <- vglm(q_tot_npk ~ k_home + k_social + hh_education, genpoisson, data = data_amhara)
summary(reg_amhara)
AICc(reg_amhara)

#Model2
reg_amhara_1 <- vglm(q_tot_npk ~ k_home + k_social + hh_education + offfarm_income + euc, genpoisson, data = data_amhara)
summary(reg_amhara_1)
AICc(reg_amhara_1)

#Model3
reg_amhara_2 <- vglm(q_tot_npk ~ k_home + k_social + hh_education + offfarm_income + hh_age + euc + hh_gender, genpoisson, data = data_amhara)
summary(reg_amhara_2)
AICc(reg_amhara_2)

# Computation for sub-region: Tigray

#Model1
reg_tigray <- vglm(q_tot_npk ~ k_home + k_social +  hh_education, genpoisson, data = data_tigray)
summary(reg_tigray)
AICc(reg_tigray)

#Model2
reg_tigray_1 <- vglm(q_tot_npk ~ k_home + k_social +  hh_education + offfarm_income + euc, genpoisson, data = data_tigray)
summary(reg_tigray_1)
AICc(reg_tigray_1)

#Model3
reg_tigray_2 <- vglm(q_tot_npk ~ k_home + k_social +  hh_education + offfarm_income + euc + hh_gender + hh_age, genpoisson, data = data_tigray)
summary(reg_tigray_2)
AICc(reg_tigray_2)

# Here it is possible to compute the effects of the soil ability directly on the soil parameters 
# OLS regressions

# Both regions of the study together

reg_all_AvailP <- lm(AvailP ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data)
summary(reg_all_AvailP)

reg_all_AvailK <- lm(AvailK ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data)
summary(reg_all_AvailK)

reg_all_TN <- lm(TN ~ k_home + k_social + hh_education  + OM + clay + gps_alt + claypsilt, data = data)
summary(reg_all_TN)


# Sub-region specific: Amhara

P_am <- lm(AvailP ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_amhara)
summary(P_am)

K_am <- lm(AvailK ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_amhara)
summary(K_am)

TN_am <- lm(TN ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_amhara)
summary(TN_am)

# Sub - region specific: Tigray

P_tig1 <- lm(AvailP ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_tigray)
summary(P_tig1)

P_tig2 <- lm(AvailK ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_tigray)
summary(P_tig2)

P_tig3 <- lm(TN ~ k_home + k_social + hh_education + OM + clay + gps_alt + claypsilt, data = data_tigray)
summary(P_tig3)

# End file