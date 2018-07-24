# Lab 5: Lab 5: HPV Vaccination Completion Rates (Logistic Regression)
# Mehran Ghandehari
# March 7th


# Load Packages and Libraries:
library(boot)
library(car)
library(MASS)
library(knitr)
library(leaps)

# Load Data
gardasil = read.table("~/Desktop/Quantitative/Lab5/jh_gardasil.dat", header=TRUE)

# we noticed that there are a descrepency between the variable completed and number of shots. So we decided 
# to correct the completed vaiable based upon the number of shots that the patients completed.
gardasil$Completed = ifelse(gardasil$Shots == 3 ,1, gardasil$Completed)

# Summarize Data 
summary(gardasil)
##############Make Dummy Variables###################
# in order to use subcategories in each vaibale as independent vaiables in our regression we created dummy vaiable
# for each sub-category.

# Dummy Variables: Age Group
gardasil$Yrs11_17 = ifelse(gardasil$AgeGroup == 0, c(1), c(0)) #11-17 years = 1 in Dummy
gardasil$Yrs18_26 = ifelse(gardasil$AgeGroup == 1, c(1), c(0)) #18-26 years = 1 in Dummy

# Dummy Variables: Race
gardasil$White = ifelse(gardasil$Race == 0, c(1), c(0))
gardasil$Black = ifelse(gardasil$Race == 1, c(1), c(0))
gardasil$Hispanic = ifelse(gardasil$Race == 2, c(1), c(0))
gardasil$Other = ifelse(gardasil$Race == 3, c(1), c(0))

# Dummy Variables: Insurance Type
gardasil$MedicalAssist = ifelse(gardasil$InsuranceType == 0, c(1), c(0))
gardasil$PrivatePayer = ifelse(gardasil$InsuranceType == 1, c(1), c(0))
gardasil$HospitalBased = ifelse(gardasil$InsuranceType == 2, c(1), c(0))
gardasil$Military = ifelse(gardasil$InsuranceType == 3, c(1), c(0))

# Dummy Variables: Reverse Medical Assistance
gardasil$NoMedAssist = ifelse(gardasil$MedAssist == 0, c(1), c(0)) 
#0 = No NonMedAsst, 1 = Yes No MedAssist in Dummy

# Dummy Variables: Location
gardasil$Odenton = ifelse(gardasil$Location == 1, c(1), c(0))
gardasil$WhiteMarsh = ifelse(gardasil$Location == 2, c(1), c(0))
gardasil$JohnsHopkins = ifelse(gardasil$Location == 3, c(1), c(0))
gardasil$Bayview = ifelse(gardasil$Location == 4, c(1), c(0))

# Dummy Variables: Reverse Location Type
gardasil$Suburban = ifelse(gardasil$Location == 0, c(1), c(0)) # 1 = Suburban, 0 = Urban in Dummy

# Dummy Variables: Practice Type
gardasil$Pediatric = ifelse(gardasil$PracticeType == 0, c(1), c(0))
gardasil$FamilyPractice = ifelse(gardasil$PracticeType == 1, c(1), c(0))
gardasil$OBGYN = ifelse(gardasil$PracticeType == 2, c(1), c(0))

# Check MedAssist matches MedicalAssist Insurance Type
ComparisonMedAsst = ifelse(gardasil$MedicalAssist == gardasil$MedAssist, c(0), c(1)) # Counts mismatches 
sum(ComparisonMedAsst) # If mismatch occurs, sum will be > 0. If not, the two are equal)
# Test determines MedAssist and MedicalAssist Insurance Type are the same.

################################
# Converting the categorical variables to factor

for (g in which(names(gardasil)!= "Age")){
  gardasil[,g] = as.factor(gardasil[,g])
}


#assign names to the dataframe
gardasil$MedAssist = factor(gardasil$MedAssist, 
                             levels=c(0,1),  
                             labels=c("NO_Asst", "Asst")) 
gardasil$Location = factor(gardasil$Location, 
                            levels = c(1,2,3,4),  
                            labels=c("Odenton", "WhiteMarsh", "JohnsHopkins",  "Bayview")) 
gardasil$LocationType = factor(gardasil$LocationType, 
                                levels=c(0,1), labels=c("Suburban", "Urban")) 
gardasil$Completed = factor(gardasil$Completed, 
                             levels=c(0,1), labels=c("No", "Yes")) 
gardasil$Race = factor(gardasil$Race, 
                        levels=c(0,1,2,3),  
                        labels=c("White", "Black", "Hispanic", "Other"))
gardasil$InsuranceType = factor(gardasil$InsuranceType, 
                        levels=c(0,1,2,3),  
                        labels=c("MedAssis", "Private", "Hospital", "Military"))
gardasil$PracticeType = factor(gardasil$PracticeType, 
                                 levels=c(1,2,3),  
                                 labels=c("Pediatric", "FamilyPrac", "OB_GYN"))
gardasil$AgeGroup = factor(gardasil$AgeGroup, 
                                levels=c(0,1),  
                                labels=c("Yrs11_17", "Yrs18_26"))

# Summarize Data 
# here is a smmary of our data after creating dummy variables, Converting the categorical variables to factor
# and assigning names to the variables
summary(gardasil)
################################

# percentage of completed vs incompleted vaccines
summary(gardasil$Completed) / sum(summary(gardasil$Completed))
#  No       Yes 
# 0.6680821 0.3319179 
################ Bivariate Logistic Regression Models#################

#completion rates by location 
locs= table(gardasil$Completed,gardasil$Location) 
kable(prop.table(locs, 2) *100, digits = 1) # column percentages
barplot(prop.table(locs, 2), beside=TRUE)
glmLocation=glm(gardasil$Completed~gardasil$Location, family=binomial) 
kable(exp(cbind(OR=coef(glmLocation),confint(glmLocation))), digits = 4)
summary(glmLocation)
plot(gardasil$Location, fitted(glmLocation), 
     main = "Probability of Vaccine Regimen Completion By Location", 
     xlab = "Location", ylab = "Gardasil Completion Probability")

# Based on the results we can conclude that patients who go to the Johns Hopkins clinic have the lowest rate
# of completion and other other hand approximately half of the patients who go to the White Marsh would complete
# their vaccination.
################################
#completion rates by Race 
race= table(gardasil$Completed,gardasil$Race) 
kable(prop.table(race, 2) *100, digits = 1) # column percentages
barplot(prop.table(race, 2), beside=TRUE)
glmRace=glm(gardasil$Completed~gardasil$Race, family=binomial) 
kable(exp(cbind(OR=coef(glmRace),confint(glmRace))), digits = 4)
summary(glmRace)
plot(gardasil$Race, fitted(glmRace), 
     main = "Probability of Vaccine Regimen Completion By Race", 
     xlab = "Race", ylab = "Gardasil Completion Probability")
# Based on the results, the race black is a significant predictor and the probability of completion is the
# the lowest (~ 29%). By the way, the probabilty that a person without Medical Assistance complete the vaccinatin
# is double a person with Medical Assistance
################################
#completion rates by Medical Assistance 
MedAssist= table(gardasil$Completed,gardasil$MedAssist) 
kable(prop.table(MedAssist, 2) *100, digits = 1) # column percentages
barplot(prop.table(MedAssist, 2), beside=TRUE)
glmMedAssist=glm(gardasil$Completed~gardasil$MedAssist , family=binomial) 
kable(exp(cbind(OR=coef(glmMedAssist),confint(glmMedAssist))), digits = 4)
summary(glmMedAssist)
plot(gardasil$MedAssist, fitted(glmMedAssist), 
     main = "Probability of Vaccine Regimen Completion by Medical Assistance", 
     xlab = "Medical Assistance", ylab = "Gardasil Completion Probability")


# It seems that the patients who use the medical assistace are less likely to finish 
# the Gardasil regimen (probability of 25%).

################################
#completion rates by Location Type
LocationType= table(gardasil$Completed,gardasil$LocationType) 
kable(prop.table(LocationType, 2) *100, digits = 1) # column percentages
glmLocationType=glm(gardasil$Completed~gardasil$LocationType , family=binomial) 
kable(exp(cbind(OR=coef(glmLocationType),confint(glmLocationType))), digits = 4)
summary(glmLocationType)
plot(gardasil$LocationType, fitted(glmLocationType), 
     main = "Probability of Vaccine Regimen Completion By Location Type", 
     xlab = "Location Type", ylab = "Gardasil Completion Probability")

# Location type is also a dignificant predictor. Those who go to to the urban clinics are less likely finish 
# the Gardasil regimen (probability of 29%). That is, the probablility of completion rate is a suburban clinic
# double of a urban clinic.

################################
#completion rates by Insurance Type
InsuranceType= table(gardasil$Completed,gardasil$InsuranceType) 
kable(prop.table(InsuranceType, 2) *100, digits = 1) # column percentages
glmInsuranceType=glm(gardasil$Completed~gardasil$InsuranceType , family=binomial) 
kable(exp(cbind(OR=coef(glmInsuranceType),confint(glmInsuranceType))), digits = 4)
summary(glmInsuranceType)
plot(gardasil$InsuranceType, fitted(glmInsuranceType), 
     main = "Probability of Vaccine Regimen Completion By Insurance Type", 
     xlab = "Insurance Type", ylab = "Gardasil Completion Probability")

# as we saw before Medical Assistance is a significant predictor. It seems that PrivatePayer is also can be 
# considerede as a signficant predictor. The probability that PrivatePayer patients will complete the 
# Gardasil regimen is about 38%. 

################################
#completion rates by Practice Type
PracticeType= table(gardasil$Completed,gardasil$PracticeType) 
kable(prop.table(PracticeType, 2) *100, digits = 1) # column percentages
glmPracticeType=glm(gardasil$Completed~gardasil$PracticeType , family=binomial) 
kable(exp(cbind(OR=coef(glmPracticeType),confint(glmPracticeType))), digits = 4)
summary(glmPracticeType)
plot(gardasil$PracticeType, fitted(glmPracticeType), 
     main = "Probability of Vaccine Regimen Completion By Practice Type", 
     xlab = "Practice Type", ylab = "Gardasil Completion Probability")

# only 32% of the Pediatric group complete the vaccination.
################################
#completion rates by AgeGroup
AgeGroup= table(gardasil$Completed,gardasil$AgeGroup) 
kable(prop.table(AgeGroup, 2) *100, digits = 1) # column percentages
glmAG=glm(gardasil$Completed~gardasil$AgeGroup, family=binomial) 
kable(exp(cbind(OR=coef(glmAG),confint(glmAG))), digits = 4)
summary(glmAG)
plot(gardasil$AgeGroup, fitted(glmAG), 
     main = "Probability of Vaccine Regimen Completion By Age Group", 
     xlab = "Age Group", ylab = "Gardasil Completion Probability")

# The Probability of Gardasil Vaccine Regimen Completion does not appear to be significantly
# affected by AgeGroup. The Probability Decreases slightly with the Older AgeGroup Aged 18-26, given the data. 
################################
#completion rates by Age
Age= table(gardasil$Completed,gardasil$Age) 
kable(prop.table(Age, 2) *100, digits = 1) # column percentages
boxplot(Age ~ Completed, data=gardasil)
glmAge=glm(gardasil$Completed~gardasil$Age, family=binomial) 
kable(exp(cbind(OR=coef(glmAge),confint(glmAge))), digits = 4)
summary(glmAge)
plot(gardasil$Age, fitted(glmAge), main = "Probability of Vaccine Regimen Completion By Age", 
     xlab = "Patient Age (in Years)", ylab = "Gardasil Completion Probability")
# Probability of completion does  appear to be affected by age.
# The probability decreases slightly with age in this data. 
# There is a negative (or inverse) relationship between completion of the Gardasil 
# Vaccine Regimen and Age. 


#plot(glmInsuranceType)
#plot(predict(glmAG, type="response"), residuals(glmAG, type= "deviance"))
##############test###################

anova(glmAge, test = "LRT")
plot(anova(glmAge, test = "LRT"))
# Compare two models:
anova(fit.reduced, fit.full, test="Chisq")

"The nonsignificant chi-square value (p = 0.21) suggests that the reduced model with
four predictors fits as well as the full model with nine predictors, reinforcing your
belief that gender, children, education, and occupation don’t add significantly to the
prediction above and beyond the other variables in the equation. Therefore, you can
base your interpretations on the simpler model."

regsubsets() # find the best combination for the Multivariate Regression


##############Multivariate Logistic Regression Models###################
# we begin with a model with all of the parametes that were significant in our bivariate logistic regression models
# because both Johns Hopkins and Bayview were significant and urban, I just used LocationType (urban = 1).

# Null hypothesis: the probability of a patient's vaccine completion is no better than the average probabiliy.
fit.full = glm(Completed ~ MedAssist + Black + LocationType + Pediatric + PrivatePayer + gardasil$Age + 
                HospitalBased + Hispanic + FamilyPractice, data=gardasil, family="binomial")
summary (fit.full)
anova(fit.full, test = "LRT")
plot(anova(fit.full, test = "LRT"))


#From the p-values for the regression coefficients (last column), you can see that 
# PrivatePayer and Hispanic may not make a significant con- tribution to the equation 
# (you can’t reject the hypothesis that the parameters are 0).
#  are the least significant for both the Wald and LRT tests. We Will eliminate them in our next model.
#AIC: 1806.9

lmtest:::lrtest(fit.full)
#The chi-square of 89.825 with 9 degrees of freedom and an associated p-value of significantly less 
#than 0.001 tells us that our model as a whole fits significantly better than an empty model. 

fit.reduced1 = glm(Completed ~ MedAssist + Black + LocationType + gardasil$Age + 
                HospitalBased + FamilyPractice + Pediatric, data=gardasil, family="binomial")
summary (fit.reduced1)
anova(fit.reduced1, test = "LRT")
# AIC: 1803
# Pediatric is the least significant for both the Wald and LRT tests. We Will eliminate them.

fit.reduced2 = glm(Completed ~ MedAssist + Black + LocationType + gardasil$Age + 
                HospitalBased + FamilyPractice, data=gardasil, family="binomial")
summary (fit.reduced2)
anova(fit.reduced2, test = "LRT")
# AIC: 1803.6
# Each regression coefficient in this reduced model is statistically significant 
# The mylogit3 (reduced) model has a similar AIC to mylogit2, so keep it.

drop1(fit.reduced2, test = "LRT")
# Null hypothesis: There is no difference in the deviance between the two models.
anova(fit.reduced1, fit.reduced2, test = "Chisq")
# All of the variables are sygnificant. It means that there is a sygnificant difference in the deviance 
# between the reduced and full models. So, we do not drop any variabels from our model. Also anova test
# using Chisq method illustrate that there not a sygnificant difference between model mylogit2 and mylogit3. SSoils
# we keep the reduced model

#interaction model
fit.reduced3 = glm(Completed ~ MedAssist + Black + Age + 
                 FamilyPractice + HospitalBased * LocationType, data=gardasil, family="binomial")
summary (fit.reduced3)
anova(fit.reduced3, test = "LRT")
#AIC: 1802.8
anova(fit.reduced2, fit.reduced3, test = "Chisq")
kable(exp(cbind(OR=coef(fit.reduced3),confint(fit.reduced3))), digits = 4)

BIC(fit.reduced2)
BIC(fit.reduced3)

fit.reduced4 = glm(Completed ~ MedAssist + Black + Age * LocationType + 
                     FamilyPractice + HospitalBased * LocationType, data=gardasil, family="binomial")
summary (fit.reduced4)
anova(fit.reduced3, test = "LRT")

#############################


  




