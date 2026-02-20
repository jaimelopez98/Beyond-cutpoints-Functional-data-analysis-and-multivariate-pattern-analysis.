# Script name: 07_INTERACTIONS.R

# Author: J.López, Inserm

# Doing: 
#   * Check the interactions between the age group and the time spent in each activity intensity separately in youth and adulthood
#     using the cut-point and the functional approaches. 

# PACKAGES ----

library(refund)

# 1) YOUTH (3-17y)----

# DATA LOADING ----
data <- read.csv("path/youth_data_ref.csv")
load("path/data_youth.rda")

pts <- round(seq(1:2000))

# CUTPOINT APPROACH 

# interaction variables
data$age1_x_SB1 <- data$age1 * data$SB1_min_day
data$age1_x_SB2 <- data$age1 * data$SB2_min_day
data$age1_x_SB3 <- data$age1 * data$SB3_min_day

data$age1_x_LIPA1 <- data$age1 * data$LIPA1_min_day
data$age1_x_LIPA2 <- data$age1 * data$LIPA2_min_day
data$age1_x_LIPA3 <- data$age1 * data$LIPA3_min_day

data$age1_x_MPA1 <- data$age1 * data$MPA1_min_day
data$age1_x_MPA2 <- data$age1 * data$MPA2_min_day
data$age1_x_MPA3 <- data$age1 * data$MPA3_min_day

data$age1_x_VPA1 <- data$age1 * data$VPA1_min_day
data$age1_x_VPA2 <- data$age1 * data$VPA2_min_day
data$age1_x_VPA3 <- data$age1 * data$VPA3_min_day

data$age1_x_MVPA1 <- data$age1 * data$MVPA1_min_day
data$age1_x_MVPA2 <- data$age1 * data$MVPA2_min_day
data$age1_x_MVPA3 <- data$age1 * data$MVPA3_min_day

data$age2_x_SB1 <- data$age2 * data$SB1_min_day
data$age2_x_SB2 <- data$age2 * data$SB2_min_day
data$age2_x_SB3 <- data$age2 * data$SB3_min_day

data$age2_x_LIPA1 <- data$age2 * data$LIPA1_min_day
data$age2_x_LIPA2 <- data$age2 * data$LIPA2_min_day
data$age2_x_LIPA3 <- data$age2 * data$LIPA3_min_day

data$age2_x_MPA1 <- data$age2 * data$MPA1_min_day
data$age2_x_MPA2 <- data$age2 * data$MPA2_min_day
data$age2_x_MPA3 <- data$age2 * data$MPA3_min_day

data$age2_x_VPA1 <- data$age2 * data$VPA1_min_day
data$age2_x_VPA2 <- data$age2 * data$VPA2_min_day
data$age2_x_VPA3 <- data$age2 * data$VPA3_min_day

data$age2_x_MVPA1 <- data$age2 * data$MVPA1_min_day
data$age2_x_MVPA2 <- data$age2 * data$MVPA2_min_day
data$age2_x_MVPA3 <- data$age2 * data$MVPA3_min_day

#models without interactions

lm_SB1_no <-  lm(bmi ~ SB1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)
lm_SB2_no <-  lm(bmi ~ SB2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)
lm_SB3_no <-  lm(bmi ~ SB3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)

lm_LIPA1_no <-  lm(bmi ~ LIPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_LIPA2_no <-  lm(bmi ~ LIPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_LIPA3_no <-  lm(bmi ~ LIPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)

lm_MPA1_no <-  lm(bmi ~ MPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_MPA2_no <-  lm(bmi ~ MPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_MPA3_no <-  lm(bmi ~ MPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)

lm_VPA1_no <-  lm(bmi ~ VPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_VPA2_no <-  lm(bmi ~ VPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_VPA3_no <-  lm(bmi ~ VPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)

lm_MVPA1_no <-  lm(bmi ~ MVPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_MVPA2_no <-  lm(bmi ~ MVPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_MVPA3_no <-  lm(bmi ~ MVPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)

# models with interactions

lm_SB1_yes <-  lm(bmi ~ SB1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB1 + age2_x_SB1, data = data)
lm_SB2_yes <-  lm(bmi ~ SB2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB2 + age2_x_SB2, data = data)
lm_SB3_yes <-  lm(bmi ~ SB3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB3 + age2_x_SB3, data = data)

lm_LIPA1_yes <-  lm(bmi ~ LIPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA1 + age2_x_LIPA1, data = data)
lm_LIPA2_yes <-  lm(bmi ~ LIPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA2 + age2_x_LIPA2, data = data)
lm_LIPA3_yes <-  lm(bmi ~ LIPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA3 + age2_x_LIPA3, data = data)

lm_MPA1_yes <-  lm(bmi ~ MPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA1 + age2_x_MPA1, data = data)
lm_MPA2_yes <-  lm(bmi ~ MPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA2 + age2_x_MPA2, data = data)
lm_MPA3_yes <-  lm(bmi ~ MPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA3 + age2_x_MPA3, data = data)

lm_VPA1_yes <-  lm(bmi ~ VPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA1 + age2_x_VPA1, data = data)
lm_VPA2_yes <-  lm(bmi ~ VPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA2 + age2_x_VPA2, data = data)
lm_VPA3_yes <-  lm(bmi ~ VPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA3 + age2_x_VPA3, data = data)

lm_MVPA1_yes <-  lm(bmi ~ MVPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA1 + age2_x_MVPA1, data = data)
lm_MVPA2_yes <-  lm(bmi ~ MVPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA2 + age2_x_MVPA2, data = data)
lm_MVPA3_yes <-  lm(bmi ~ MVPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA3 + age2_x_MVPA3, data = data)

anova(lm_SB1_no, lm_SB1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_SB2_no, lm_SB2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_SB3_no, lm_SB3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_LIPA1_no, lm_LIPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_LIPA2_no, lm_LIPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_LIPA3_no, lm_LIPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_MPA1_no, lm_MPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MPA2_no, lm_MPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MPA3_no, lm_MPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_VPA1_no, lm_VPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_VPA2_no, lm_VPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_VPA3_no, lm_VPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_MVPA1_no, lm_MVPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MVPA2_no, lm_MVPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MVPA3_no, lm_MVPA3_yes, test="LRT")$`Pr(>Chi)`

summary(lm_SB1_yes)
summary(lm_SB2_yes)
summary(lm_SB3_yes)

summary(lm_LIPA1_yes)
summary(lm_LIPA2_yes)
summary(lm_LIPA3_yes)

summary(lm_MPA1_yes)
summary(lm_MPA2_yes)
summary(lm_MPA3_yes)

summary(lm_VPA1_yes)
summary(lm_VPA2_yes)
summary(lm_VPA3_yes)

summary(lm_MVPA1_yes)
summary(lm_MVPA2_yes)
summary(lm_MVPA3_yes)

# FUNCTIONAL APPROACH

# including Age as continuous

# interaction variables
data_youth$age1_x_PA <- data_youth$age1 * data_youth$Y_Activity
data_youth$age2_x_PA <- data_youth$age2 * data_youth$Y_Activity

fm_no <- pfr(bmi ~ lf(Y_Activity, argvals= pts) +
               ACC_0_0 +
               age + age1 + age2 +
               gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 +
               study_cat1 + study_cat2 + study_cat3,
             method = "REML", data=data_youth) 
summary(fm_no)
fm_yes <- pfr(bmi ~ lf(Y_Activity, argvals= pts) + 
                ACC_0_0 +
                age + age1 + age2 + 
                gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 +
                study_cat1 + study_cat2 + study_cat3 + 
                lf(age1_x_PA, argvals= pts) + lf(age2_x_PA, argvals= pts),
              method = "REML", data=data_youth) 
summary(fm_yes)

anova(fm_no, fm_yes, test="LRT")$`Pr(>Chi)`



# 2) ADULTHOOD (18-90y)----

# DATA LOADING
data <- read.csv("path/adulthood_data_ref.csv")
load("path/data_adulthood.rda")

pts <- round(seq(1:2000))

# CUTPOINT APPROACH

# interaction variables
data$age1_x_SB1 <- data$age1 * data$SB1_min_day
data$age1_x_SB2 <- data$age1 * data$SB2_min_day
data$age1_x_SB3 <- data$age1 * data$SB3_min_day

data$age1_x_LIPA1 <- data$age1 * data$LIPA1_min_day
data$age1_x_LIPA2 <- data$age1 * data$LIPA2_min_day
data$age1_x_LIPA3 <- data$age1 * data$LIPA3_min_day

data$age1_x_MPA1 <- data$age1 * data$MPA1_min_day
data$age1_x_MPA2 <- data$age1 * data$MPA2_min_day
data$age1_x_MPA3 <- data$age1 * data$MPA3_min_day

data$age1_x_VPA1 <- data$age1 * data$VPA1_min_day
data$age1_x_VPA2 <- data$age1 * data$VPA2_min_day
data$age1_x_VPA3 <- data$age1 * data$VPA3_min_day

data$age1_x_MVPA1 <- data$age1 * data$MVPA1_min_day
data$age1_x_MVPA2 <- data$age1 * data$MVPA2_min_day
data$age1_x_MVPA3 <- data$age1 * data$MVPA3_min_day

data$age2_x_SB1 <- data$age2 * data$SB1_min_day
data$age2_x_SB2 <- data$age2 * data$SB2_min_day
data$age2_x_SB3 <- data$age2 * data$SB3_min_day

data$age2_x_LIPA1 <- data$age2 * data$LIPA1_min_day
data$age2_x_LIPA2 <- data$age2 * data$LIPA2_min_day
data$age2_x_LIPA3 <- data$age2 * data$LIPA3_min_day

data$age2_x_MPA1 <- data$age2 * data$MPA1_min_day
data$age2_x_MPA2 <- data$age2 * data$MPA2_min_day
data$age2_x_MPA3 <- data$age2 * data$MPA3_min_day

data$age2_x_VPA1 <- data$age2 * data$VPA1_min_day
data$age2_x_VPA2 <- data$age2 * data$VPA2_min_day
data$age2_x_VPA3 <- data$age2 * data$VPA3_min_day

data$age2_x_MVPA1 <- data$age2 * data$MVPA1_min_day
data$age2_x_MVPA2 <- data$age2 * data$MVPA2_min_day
data$age2_x_MVPA3 <- data$age2 * data$MVPA3_min_day

#models without interactions

lm_SB1_no <-  lm(bmi ~ SB1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)
lm_SB2_no <-  lm(bmi ~ SB2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)
lm_SB3_no <-  lm(bmi ~ SB3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                 data = data)

lm_LIPA1_no <-  lm(bmi ~ LIPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_LIPA2_no <-  lm(bmi ~ LIPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_LIPA3_no <-  lm(bmi ~ LIPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)

lm_MPA1_no <-  lm(bmi ~ MPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_MPA2_no <-  lm(bmi ~ MPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_MPA3_no <-  lm(bmi ~ MPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)

lm_VPA1_no <-  lm(bmi ~ VPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_VPA2_no <-  lm(bmi ~ VPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)
lm_VPA3_no <-  lm(bmi ~ VPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                  data = data)

lm_MVPA1_no <-  lm(bmi ~ MVPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_MVPA2_no <-  lm(bmi ~ MVPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)
lm_MVPA3_no <-  lm(bmi ~ MVPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3,
                   data = data)

# models with interactions

lm_SB1_yes <-  lm(bmi ~ SB1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB1 + age2_x_SB1, data = data)
lm_SB2_yes <-  lm(bmi ~ SB2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB2 + age2_x_SB2, data = data)
lm_SB3_yes <-  lm(bmi ~ SB3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                    age1_x_SB3 + age2_x_SB3, data = data)

lm_LIPA1_yes <-  lm(bmi ~ LIPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA1 + age2_x_LIPA1, data = data)
lm_LIPA2_yes <-  lm(bmi ~ LIPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA2 + age2_x_LIPA2, data = data)
lm_LIPA3_yes <-  lm(bmi ~ LIPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_LIPA3 + age2_x_LIPA3, data = data)

lm_MPA1_yes <-  lm(bmi ~ MPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA1 + age2_x_MPA1, data = data)
lm_MPA2_yes <-  lm(bmi ~ MPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA2 + age2_x_MPA2, data = data)
lm_MPA3_yes <-  lm(bmi ~ MPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_MPA3 + age2_x_MPA3, data = data)

lm_VPA1_yes <-  lm(bmi ~ VPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA1 + age2_x_VPA1, data = data)
lm_VPA2_yes <-  lm(bmi ~ VPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA2 + age2_x_VPA2, data = data)
lm_VPA3_yes <-  lm(bmi ~ VPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                     age1_x_VPA3 + age2_x_VPA3, data = data)

lm_MVPA1_yes <-  lm(bmi ~ MVPA1_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA1 + age2_x_MVPA1, data = data)
lm_MVPA2_yes <-  lm(bmi ~ MVPA2_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA2 + age2_x_MVPA2, data = data)
lm_MVPA3_yes <-  lm(bmi ~ MVPA3_min_day + wear_min_day + age + age1 + age2 + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + study_cat3 + season_cat1 + season_cat2 + season_cat3 + 
                      age1_x_MVPA3 + age2_x_MVPA3, data = data)

anova(lm_SB1_no, lm_SB1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_SB2_no, lm_SB2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_SB3_no, lm_SB3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_LIPA1_no, lm_LIPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_LIPA2_no, lm_LIPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_LIPA3_no, lm_LIPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_MPA1_no, lm_MPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MPA2_no, lm_MPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MPA3_no, lm_MPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_VPA1_no, lm_VPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_VPA2_no, lm_VPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_VPA3_no, lm_VPA3_yes, test="LRT")$`Pr(>Chi)`

anova(lm_MVPA1_no, lm_MVPA1_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MVPA2_no, lm_MVPA2_yes, test="LRT")$`Pr(>Chi)`
anova(lm_MVPA3_no, lm_MVPA3_yes, test="LRT")$`Pr(>Chi)`

summary(lm_SB1_yes)
summary(lm_SB2_yes)
summary(lm_SB3_yes)

summary(lm_LIPA1_yes)
summary(lm_LIPA2_yes)
summary(lm_LIPA3_yes)

summary(lm_MPA1_yes)
summary(lm_MPA2_yes)
summary(lm_MPA3_yes)

summary(lm_VPA1_yes)
summary(lm_VPA2_yes)
summary(lm_VPA3_yes)

summary(lm_MVPA1_yes)
summary(lm_MVPA2_yes)
summary(lm_MVPA3_yes)

# FUNCTIONAL APPROACH 

# including Age as continuous

# interaction variables
data_adulthood$age1_x_PA <- data_adulthood$age1 * data_adulthood$Y_Activity
data_adulthood$age2_x_PA <- data_adulthood$age2 * data_adulthood$Y_Activity

fm_no <- pfr(bmi ~ lf(Y_Activity, argvals= pts) + 
               ACC_0_0 +
               age + age1 + age2 +
               gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 +
               study_cat1 + study_cat2 + study_cat3,
             method = "REML", data=data_adulthood) 
summary(fm_no)
fm_yes <- pfr(bmi ~ lf(Y_Activity, argvals= pts) + 
                ACC_0_0 +
                age + age1 + age2 + 
                gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 +
                study_cat1 + study_cat2 + study_cat3 + 
                lf(age1_x_PA, argvals= pts) + lf(age2_x_PA, argvals= pts),
              method = "REML", data=data_adulthood) 
summary(fm_yes)

anova(fm_no, fm_yes, test="LRT")$`Pr(>Chi)`