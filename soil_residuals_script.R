## Smallholder households' farming knowledge affects soil ability in marginal areas 
## Occelli, Mantino, Ragaglini, Dell'Acqua, Pè and Nuvolari

# Script for the computation of soil residuals, stemming from soil analysis

# Load libraries
library(readxl)
library(ggplot2)
library(MASS)

# Set your working directory

# Load soil data
data_soil <- read_excel("data_soil.xlsx", 
                                col_types = c("numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "text", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", "numeric", "numeric"))
View(data_soil)

# Create sub-dataset for Amhara (hhid < 5500000)
dataset_amhara <- subset(data_soil, Dataset_soil_join$hhid<5500000)
View(dataset_amhara)

# Stepwise regression

# N
full.model_tn <- lm(TN ~ gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = dataset_amhara)
step.model_tn <- stepAIC(full.model_tn, direction = "both", trace = F)
summary(step.model_tn)
step_res_tn <- resid(step.model_tn)

min(step_res_tn)
max(step_res_tn)
sd(step_res_tn, na.rm = T)

# Calculate quantile
boxplot(step_res_tn)
q_tn <- quantile(step_res_tn)
q_tn
q_tn_sw <- cut(step_res_tn, q_tn, labels = F, include.lowest = T)
View(q_tn_sw)

# Calculate residuals
p_sw <- step.model_tn["residuals"]
f_sw <- step.model_tn["fitted.values"]
e_sw <- step.model_tn["effects"]

tn_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_tn_sw, TN= dataset_amhara$TN)
View(tn_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="TN_Amhara_residuals_qt.xlsx", sheetName = "Amhara Residual of TN")

# P
full.model_p <- lm(AvailP ~ OM + gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = dataset_amhara)
step.model_p <- stepAIC(full.model_p, direction = "both", trace = F)
summary(step.model_p)
step_res_p <- resid(step.model_p)

min(step_res_p)
max(step_res_p)
sd(step_res_p)

# Calculate quantile
boxplot(step_res_p)
q_p <- quantile(step_res_p)
q_p
q_p_sw <- cut(step_res_p, q_p, labels = F, include.lowest = T)
View(q_p_sw)

# Calculate residuals
p_sw <- step.model_p["residuals"]
f_sw <- step.model_p["fitted.values"]
e_sw <- step.model_p["effects"]

p_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_p_sw, AvailP= dataset_amhara$AvailP)
View(p_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="AvailP_Amhara_residuals_qt.xlsx", sheetName = "Amhara Residual of AvailP")

# K
full.model_k <- lm(AvailK ~ OM + gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = dataset_amhara)
step.model_K <- stepAIC(full.model_k, direction = "both", trace = F)
summary(step.model_K)
step_res_k <- resid(step.model_K)

min(step_res_k)
max(step_res_k)
sd(step_res_k)

# Calculate quantile

boxplot(step_res_k)
q_k <- quantile(step_res_k)
View(q_k)
q_k_sw <- cut(step_res_k, q_k, labels = F, include.lowest = T)
View(q_k_sw)

# Calculate residuals
p_sw <- step.model_K["residuals"]
f_sw <- step.model_K["fitted.values"]
e_sw <- step.model_K["effects"]

k_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_k_sw, AvailK= dataset_amhara$AvailK)
View(k_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="AvailK_Amhara_residuals_qt.xlsx", sheetName = "Amhara Residual of AvailK")

# Now repeat the same procedure for Tigray (hhid>5500000)
dataset_tigray <- subset(data_soil, Dataset_soil_join$hhid>5500000)
View(dataset_tigray)

# Clean Tigray data
data_tigray <- subset(dataset_tigray[c(2:5, 7:16,26)])
View(data_tigray)
anyNA(data_tigray)
data_tig <- na.omit(data_tigray)
View(data_tig)

#N

full.model.tig_tn <- lm(TN ~ gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = data_tig)
step.model.tig_tn <- stepAIC(full.model.tig_tn, direction = "both", trace = F)
summary(step.model.tig_tn)
steptig_res_tn <- resid(step.model.tig_tn)

min(steptig_res_tn)
max(steptig_res_tn)
sd(steptig_res_tn)

# Calculate quantile

boxplot(steptig_res_tn)
q_tn_tig <- quantile(steptig_res_tn)
q_tn_tig
q_tn_tig <- cut(steptig_res_tn, q_tn_tig, labels = F, include.lowest = T)
View(q_tn_tig)

# Calculate residuals
p_sw <- step.model.tig_tn["residuals"]
f_sw <- step.model.tig_tn["fitted.values"]
e_sw <- step.model.tig_tn["effects"]

k_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_tn_tig, TN= data_tig$TN)
View(k_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="TN_Tigray_residuals_qt.xlsx", sheetName = "Tigray Residual of TN")

#P
full.model.tig_p <- lm(AvailP ~ OM + gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = data_tig)
step.model.tig_p <- stepAIC(full.model.tig_p, direction = "both", trace = F)
summary(step.model.tig_p)
steptig_res_p <- resid(step.model.tig_p)

min(steptig_res_p)
max(steptig_res_p)
sd(steptig_res_p)

# Calculate quantile

boxplot(steptig_res_p)
q_p_tig <- quantile(steptig_res_p)
q_p_tig
q_p_tig <- cut(steptig_res_p, q_p_tig, labels = F, include.lowest = T)
View(q_p_tig)


# Calculate residuals
p_sw <- step.model.tig_p["residuals"]
f_sw <- step.model.tig_p["fitted.values"]
e_sw <- step.model.tig_p["effects"]

k_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_p_tig, AvailP= data_tig$AvailP)
View(k_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="AvailP_Tigray_residuals_q.xlsx", sheetName = "Tigray Residual of AvailP")


# K
full.model.tig_k <- lm(AvailK ~ OM + gps_alt + ph_h2o + claypsilt  + CaCo3 + EC + CEC + CN_ratio, data = data_tig)
step.model.tig_K <- stepAIC(full.model.tig_k, direction = "both", trace = F)
summary(step.model.tig_K)
steptig_res_k <- resid(step.model.tig_K)

min(steptig_res_k)
max(steptig_res_k)
sd(steptig_res_k)

# Calculate quantile

boxplot(steptig_res_k)
q_k_tig <- quantile(steptig_res_k)
q_k_tig
q_k_tig <- cut(steptig_res_k, q_k_tig, labels = F, include.lowest = T)
View(q_k_tig)


# Calculate residuals
p_sw <- step.model.tig_K["residuals"]
f_sw <- step.model.tig_K["fitted.values"]
e_sw <- step.model.tig_K["effects"]

k_sw <- cbind( res=p_sw$residuals, eff=e_sw$effects, fitt= f_sw$fitted.values, q_k_tig, AvailK= data_tig$AvailK)
View(k_sw)

# Write Excel with results
install.packages("xlsx")
library(xlsx)
write.xlsx(k_sw, file="AvailK_Tigray_residuals_qt.xlsx", sheetName = "Tigray Residual of AvailK")

# End file