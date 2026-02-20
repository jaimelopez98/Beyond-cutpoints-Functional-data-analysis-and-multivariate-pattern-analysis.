# Script name: 11_CUTPOINTS_MODELS.R

# Author: J.Lopez, Inserm

# Doing: Modelling linear regression models for each physical activity intensity from different cut-poits in each age group.
# (2026-02-23 data release)

# PACKAGES 

library(dplyr)
library(openxlsx)

data <- read.csv("path/data_ref.csv") %>%
  mutate(SB1_10_min_day = SB1_min_day/10,
         LIPA1_10_min_day = LIPA1_min_day/10,
         MPA1_10_min_day = MPA1_min_day/10,
         VPA1_10_min_day = VPA1_min_day/10,
         MVPA1_10_min_day = MVPA1_min_day/10,
         SB2_10_min_day = SB1_min_day/10,
         LIPA2_10_min_day = LIPA1_min_day/10,
         MPA2_10_min_day = MPA1_min_day/10,
         VPA2_10_min_day = VPA1_min_day/10,
         MVPA2_10_min_day = MVPA1_min_day/10,
         SB3_10_min_day = SB1_min_day/10,
         LIPA3_10_min_day = LIPA1_min_day/10,
         MPA3_10_min_day = MPA1_min_day/10,
         VPA3_10_min_day = VPA1_min_day/10,
         MVPA3_10_min_day = MVPA1_min_day/10)

presc <- data %>% filter(age<6)
child <- data %>% filter(age>=6 & age<11)
adoles <- data %>% filter(age>=11 & age<19)
younger <- data %>% filter(age>=19 & age<45)
middle <- data %>% filter(age>=45 & age<65)
older <- data %>% filter(age>=65)

# CUT-POINT 1 (Troiano and Evenson for adulthood and youth, respectively) ----

# Sedentary behaviour

lm_SB1_presc <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                   data = presc)
lm_SB1_child <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                   data = child)
lm_SB1_adoles <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = adoles)
lm_SB1_younger <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = younger)
lm_SB1_middle <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = middle)
lm_SB1_older <-  lm(bmi ~ SB1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                   data = older)

# Light Physical activity

lm_LIPA1_presc <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_LIPA1_child <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_LIPA1_adoles <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_LIPA1_younger <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_LIPA1_middle <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_LIPA1_older <-  lm(bmi ~ LIPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)

# Moderate to Vigorous Physical activity

lm_MVPA1_presc <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MVPA1_child <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MVPA1_adoles <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MVPA1_younger <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MVPA1_middle <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MVPA1_older <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)
# Moderate Physical activity

lm_MPA1_presc <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MPA1_child <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MPA1_adoles <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MPA1_younger <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MPA1_middle <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MPA1_older <-  lm(bmi ~ MPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)
# Vigorous Physical activity

lm_VPA1_presc <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_VPA1_child <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_VPA1_adoles <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_VPA1_younger <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_VPA1_middle <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_VPA1_older <-  lm(bmi ~ VPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)

# Moderate to Vigorous Physical activity

lm_MVPA1_presc <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_MVPA1_child <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_MVPA1_adoles <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_MVPA1_younger <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_MVPA1_middle <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_MVPA1_older <-  lm(bmi ~ MVPA1_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)

presc_bmi <- round(c(coef(lm_SB1_presc)[2],confint(lm_SB1_presc)[2],confint(lm_SB1_presc)[13],summary(lm_SB1_presc)$coefficients[2,4],
                     coef(lm_LIPA1_presc)[2],confint(lm_LIPA1_presc)[2],confint(lm_LIPA1_presc)[13],summary(lm_LIPA1_presc)$coefficients[2,4],
                     coef(lm_MPA1_presc)[2],confint(lm_MPA1_presc)[2],confint(lm_MPA1_presc)[13],summary(lm_MPA1_presc)$coefficients[2,4],
                     coef(lm_VPA1_presc)[2],confint(lm_VPA1_presc)[2],confint(lm_VPA1_presc)[13],summary(lm_VPA1_presc)$coefficients[2,4],
                     coef(lm_MVPA1_presc)[2],confint(lm_MVPA1_presc)[2],confint(lm_MVPA1_presc)[13],summary(lm_MVPA1_presc)$coefficients[2,4]),2)

child_bmi <- round(c(coef(lm_SB1_child)[2],confint(lm_SB1_child)[2],confint(lm_SB1_child)[14],summary(lm_SB1_child)$coefficients[2,4],
                     coef(lm_LIPA1_child)[2],confint(lm_LIPA1_child)[2],confint(lm_LIPA1_child)[14],summary(lm_LIPA1_child)$coefficients[2,4],
                     coef(lm_MPA1_child)[2],confint(lm_MPA1_child)[2],confint(lm_MPA1_child)[14],summary(lm_MPA1_child)$coefficients[2,4],
                     coef(lm_VPA1_child)[2],confint(lm_VPA1_child)[2],confint(lm_VPA1_child)[14],summary(lm_VPA1_child)$coefficients[2,4],
                     coef(lm_MVPA1_child)[2],confint(lm_MVPA1_child)[2],confint(lm_MVPA1_child)[14],summary(lm_MVPA1_child)$coefficients[2,4]),2)

adoles_bmi <- round(c(coef(lm_SB1_adoles)[2],confint(lm_SB1_adoles)[2],confint(lm_SB1_adoles)[13],summary(lm_SB1_adoles)$coefficients[2,4],
                     coef(lm_LIPA1_adoles)[2],confint(lm_LIPA1_adoles)[2],confint(lm_LIPA1_adoles)[13],summary(lm_LIPA1_adoles)$coefficients[2,4],
                     coef(lm_MPA1_adoles)[2],confint(lm_MPA1_adoles)[2],confint(lm_MPA1_adoles)[13],summary(lm_MPA1_adoles)$coefficients[2,4],
                     coef(lm_VPA1_adoles)[2],confint(lm_VPA1_adoles)[2],confint(lm_VPA1_adoles)[13],summary(lm_VPA1_adoles)$coefficients[2,4],
                     coef(lm_MVPA1_adoles)[2],confint(lm_MVPA1_adoles)[2],confint(lm_MVPA1_adoles)[13],summary(lm_MVPA1_adoles)$coefficients[2,4]),2)

younger_bmi <- round(c(coef(lm_SB1_younger)[2],confint(lm_SB1_younger)[2],confint(lm_SB1_younger)[12],summary(lm_SB1_younger)$coefficients[2,4],
                     coef(lm_LIPA1_younger)[2],confint(lm_LIPA1_younger)[2],confint(lm_LIPA1_younger)[12],summary(lm_LIPA1_younger)$coefficients[2,4],
                     coef(lm_MPA1_younger)[2],confint(lm_MPA1_younger)[2],confint(lm_MPA1_younger)[12],summary(lm_MPA1_younger)$coefficients[2,4],
                     coef(lm_VPA1_younger)[2],confint(lm_VPA1_younger)[2],confint(lm_VPA1_younger)[12],summary(lm_VPA1_younger)$coefficients[2,4],
                     coef(lm_MVPA1_younger)[2],confint(lm_MVPA1_younger)[2],confint(lm_MVPA1_younger)[12],summary(lm_MVPA1_younger)$coefficients[2,4]),2)

middle_bmi <- round(c(coef(lm_SB1_middle)[2],confint(lm_SB1_middle)[2],confint(lm_SB1_middle)[12],summary(lm_SB1_middle)$coefficients[2,4],
                     coef(lm_LIPA1_middle)[2],confint(lm_LIPA1_middle)[2],confint(lm_LIPA1_middle)[12],summary(lm_LIPA1_middle)$coefficients[2,4],
                     coef(lm_MPA1_middle)[2],confint(lm_MPA1_middle)[2],confint(lm_MPA1_middle)[12],summary(lm_MPA1_middle)$coefficients[2,4],
                     coef(lm_VPA1_middle)[2],confint(lm_VPA1_middle)[2],confint(lm_VPA1_middle)[12],summary(lm_VPA1_middle)$coefficients[2,4],
                     coef(lm_MVPA1_middle)[2],confint(lm_MVPA1_middle)[2],confint(lm_MVPA1_middle)[12],summary(lm_MVPA1_middle)$coefficients[2,4]),2)

older_bmi <- round(c(coef(lm_SB1_older)[2],confint(lm_SB1_older)[2],confint(lm_SB1_older)[12],summary(lm_SB1_older)$coefficients[2,4],
                     coef(lm_LIPA1_older)[2],confint(lm_LIPA1_older)[2],confint(lm_LIPA1_older)[12],summary(lm_LIPA1_older)$coefficients[2,4],
                     coef(lm_MPA1_older)[2],confint(lm_MPA1_older)[2],confint(lm_MPA1_older)[12],summary(lm_MPA1_older)$coefficients[2,4],
                     coef(lm_VPA1_older)[2],confint(lm_VPA1_older)[2],confint(lm_VPA1_older)[12],summary(lm_VPA1_older)$coefficients[2,4],
                     coef(lm_MVPA1_older)[2],confint(lm_MVPA1_older)[2],confint(lm_MVPA1_older)[12],summary(lm_MVPA1_older)$coefficients[2,4]),2)

sex_age <- data.frame(rbind(presc_bmi, child_bmi, adoles_bmi, younger_bmi,middle_bmi, older_bmi), row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age) <- c("SB1","2.5%", "97.5%","p.value", "LIPA1","2.5%.1", "97.5%.1", "p.value1", "MPA1","2.5%.2", "97.5%.2", "p.value2", "VPA1","2.5%.3", "97.5%.3", "p.value3", "MVPA1","2.5%.4", "97.5%.4", "p.value4")

group1 <- sprintf("%s (%s, %s) p = %s", sex_age$SB1, sex_age$`2.5%`, sex_age$`97.5%`, sex_age$p.value)
group2 <- sprintf("%s (%s, %s) p = %s", sex_age$LIPA1, sex_age$`2.5%.1`, sex_age$`97.5%.1`, sex_age$p.value1)
group3 <- sprintf("%s (%s, %s) p = %s", sex_age$MPA1, sex_age$`2.5%.2`, sex_age$`97.5%.2`, sex_age$p.value2)
group4 <- sprintf("%s (%s, %s) p = %s", sex_age$VPA1, sex_age$`2.5%.3`, sex_age$`97.5%.3`, sex_age$p.value3)
group5 <- sprintf("%s (%s, %s) p = %s", sex_age$MVPA1, sex_age$`2.5%.4`, sex_age$`97.5%.4`, sex_age$p.value4)

sex_age2 <- data.frame(group1, group2, group3, group4, group5, row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age2) <- c("SB1", "LIPA1","MPA1","VPA1","MVPA1")

write.xlsx(sex_age2, "path/cutpoint1_adjusted_15.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)

# CUT-POINT 2 (Matthews and Romanzini for adulthood and youth, respectively) ----

# Sedentary behaviour

lm_SB2_presc <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                    data = presc)
lm_SB2_child <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = child)
lm_SB2_adoles <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = adoles)
lm_SB2_younger <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = younger)
lm_SB2_middle <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = middle)
lm_SB2_older <-  lm(bmi ~ SB2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = older)

# Light Physical activity

lm_LIPA2_presc <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_LIPA2_child <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_LIPA2_adoles <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_LIPA2_younger <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_LIPA2_middle <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_LIPA2_older <-  lm(bmi ~ LIPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)

# Moderate to Vigorous Physical activity

lm_MVPA2_presc <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MVPA2_child <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MVPA2_adoles <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MVPA2_younger <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MVPA2_middle <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MVPA2_older <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)
# Moderate Physical activity

lm_MPA2_presc <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_MPA2_child <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_MPA2_adoles <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_MPA2_younger <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_MPA2_middle <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_MPA2_older <-  lm(bmi ~ MPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)
# Vigorous Physical activity

lm_VPA2_presc <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_VPA2_child <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_VPA2_adoles <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_VPA2_younger <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_VPA2_middle <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_VPA2_older <-  lm(bmi ~ VPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)

# Moderate to Vigorous Physical activity

lm_MVPA2_presc <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MVPA2_child <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MVPA2_adoles <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MVPA2_younger <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MVPA2_middle <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MVPA2_older <-  lm(bmi ~ MVPA2_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)

presc_bmi <- round(c(coef(lm_SB2_presc)[2],confint(lm_SB2_presc)[2],confint(lm_SB2_presc)[13],summary(lm_SB2_presc)$coefficients[2,4],
                     coef(lm_LIPA2_presc)[2],confint(lm_LIPA2_presc)[2],confint(lm_LIPA2_presc)[13],summary(lm_LIPA2_presc)$coefficients[2,4],
                     coef(lm_MPA2_presc)[2],confint(lm_MPA2_presc)[2],confint(lm_MPA2_presc)[13],summary(lm_MPA2_presc)$coefficients[2,4],
                     coef(lm_VPA2_presc)[2],confint(lm_VPA2_presc)[2],confint(lm_VPA2_presc)[13],summary(lm_VPA2_presc)$coefficients[2,4],
                     coef(lm_MVPA2_presc)[2],confint(lm_MVPA2_presc)[2],confint(lm_MVPA2_presc)[13],summary(lm_MVPA2_presc)$coefficients[2,4]),2)

child_bmi <- round(c(coef(lm_SB2_child)[2],confint(lm_SB2_child)[2],confint(lm_SB2_child)[14],summary(lm_SB2_child)$coefficients[2,4],
                     coef(lm_LIPA2_child)[2],confint(lm_LIPA2_child)[2],confint(lm_LIPA2_child)[14],summary(lm_LIPA2_child)$coefficients[2,4],
                     coef(lm_MPA2_child)[2],confint(lm_MPA2_child)[2],confint(lm_MPA2_child)[14],summary(lm_MPA2_child)$coefficients[2,4],
                     coef(lm_VPA2_child)[2],confint(lm_VPA2_child)[2],confint(lm_VPA2_child)[14],summary(lm_VPA2_child)$coefficients[2,4],
                     coef(lm_MVPA2_child)[2],confint(lm_MVPA2_child)[2],confint(lm_MVPA2_child)[14],summary(lm_MVPA2_child)$coefficients[2,4]),2)

adoles_bmi <- round(c(coef(lm_SB2_adoles)[2],confint(lm_SB2_adoles)[2],confint(lm_SB2_adoles)[13],summary(lm_SB2_adoles)$coefficients[2,4],
                      coef(lm_LIPA2_adoles)[2],confint(lm_LIPA2_adoles)[2],confint(lm_LIPA2_adoles)[13],summary(lm_LIPA2_adoles)$coefficients[2,4],
                      coef(lm_MPA2_adoles)[2],confint(lm_MPA2_adoles)[2],confint(lm_MPA2_adoles)[13],summary(lm_MPA2_adoles)$coefficients[2,4],
                      coef(lm_VPA2_adoles)[2],confint(lm_VPA2_adoles)[2],confint(lm_VPA2_adoles)[13],summary(lm_VPA2_adoles)$coefficients[2,4],
                      coef(lm_MVPA2_adoles)[2],confint(lm_MVPA2_adoles)[2],confint(lm_MVPA2_adoles)[13],summary(lm_MVPA2_adoles)$coefficients[2,4]),2)

younger_bmi <- round(c(coef(lm_SB2_younger)[2],confint(lm_SB2_younger)[2],confint(lm_SB2_younger)[12],summary(lm_SB2_younger)$coefficients[2,4],
                       coef(lm_LIPA2_younger)[2],confint(lm_LIPA2_younger)[2],confint(lm_LIPA2_younger)[12],summary(lm_LIPA2_younger)$coefficients[2,4],
                       coef(lm_MPA2_younger)[2],confint(lm_MPA2_younger)[2],confint(lm_MPA2_younger)[12],summary(lm_MPA2_younger)$coefficients[2,4],
                       coef(lm_VPA2_younger)[2],confint(lm_VPA2_younger)[2],confint(lm_VPA2_younger)[12],summary(lm_VPA2_younger)$coefficients[2,4],
                       coef(lm_MVPA2_younger)[2],confint(lm_MVPA2_younger)[2],confint(lm_MVPA2_younger)[12],summary(lm_MVPA2_younger)$coefficients[2,4]),2)

middle_bmi <- round(c(coef(lm_SB2_middle)[2],confint(lm_SB2_middle)[2],confint(lm_SB2_middle)[12],summary(lm_SB2_middle)$coefficients[2,4],
                      coef(lm_LIPA2_middle)[2],confint(lm_LIPA2_middle)[2],confint(lm_LIPA2_middle)[12],summary(lm_LIPA2_middle)$coefficients[2,4],
                      coef(lm_MPA2_middle)[2],confint(lm_MPA2_middle)[2],confint(lm_MPA2_middle)[12],summary(lm_MPA2_middle)$coefficients[2,4],
                      coef(lm_VPA2_middle)[2],confint(lm_VPA2_middle)[2],confint(lm_VPA2_middle)[12],summary(lm_VPA2_middle)$coefficients[2,4],
                      coef(lm_MVPA2_middle)[2],confint(lm_MVPA2_middle)[2],confint(lm_MVPA2_middle)[12],summary(lm_MVPA2_middle)$coefficients[2,4]),2)

older_bmi <- round(c(coef(lm_SB2_older)[2],confint(lm_SB2_older)[2],confint(lm_SB2_older)[12],summary(lm_SB2_older)$coefficients[2,4],
                     coef(lm_LIPA2_older)[2],confint(lm_LIPA2_older)[2],confint(lm_LIPA2_older)[12],summary(lm_LIPA2_older)$coefficients[2,4],
                     coef(lm_MPA2_older)[2],confint(lm_MPA2_older)[2],confint(lm_MPA2_older)[12],summary(lm_MPA2_older)$coefficients[2,4],
                     coef(lm_VPA2_older)[2],confint(lm_VPA2_older)[2],confint(lm_VPA2_older)[12],summary(lm_VPA2_older)$coefficients[2,4],
                     coef(lm_MVPA2_older)[2],confint(lm_MVPA2_older)[2],confint(lm_MVPA2_older)[12],summary(lm_MVPA2_older)$coefficients[2,4]),2)


sex_age <- data.frame(rbind(presc_bmi, child_bmi, adoles_bmi, younger_bmi,middle_bmi, older_bmi), row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age) <- c("SB2","2.5%", "97.5%","p.value", "LIPA2","2.5%.1", "97.5%.1", "p.value1", "MPA2","2.5%.2", "97.5%.2", "p.value2", "VPA2","2.5%.3", "97.5%.3", "p.value3", "MVPA2","2.5%.4", "97.5%.4", "p.value4")

group1 <- sprintf("%s (%s, %s) p = %s", sex_age$SB2, sex_age$`2.5%`, sex_age$`97.5%`, sex_age$p.value)
group2 <- sprintf("%s (%s, %s) p = %s", sex_age$LIPA2, sex_age$`2.5%.1`, sex_age$`97.5%.1`, sex_age$p.value1)
group3 <- sprintf("%s (%s, %s) p = %s", sex_age$MPA2, sex_age$`2.5%.2`, sex_age$`97.5%.2`, sex_age$p.value2)
group4 <- sprintf("%s (%s, %s) p = %s", sex_age$VPA2, sex_age$`2.5%.3`, sex_age$`97.5%.3`, sex_age$p.value3)
group5 <- sprintf("%s (%s, %s) p = %s", sex_age$MVPA2, sex_age$`2.5%.4`, sex_age$`97.5%.4`, sex_age$p.value4)

sex_age2 <- data.frame(group1, group2, group3, group4, group5, row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age2) <- c("SB2", "LIPA2","MPA2","VPA2","MVPA2")

write.xlsx(sex_age2, "path/cutpoint2_adjusted_15.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)

# CUT-POINT 3 (Swartz and Puyau for adulthood and youth, respectively) ----

# Sedentary behaviour

lm_SB3_presc <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                    data = presc)
lm_SB3_child <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = child)
lm_SB3_adoles <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = adoles)
lm_SB3_younger <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = younger)
lm_SB3_middle <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = middle)
lm_SB3_older <-  lm(bmi ~ SB3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                    data = older)

# Light Physical activity

lm_LIPA3_presc <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_LIPA3_child <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_LIPA3_adoles <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_LIPA3_younger <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_LIPA3_middle <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_LIPA3_older <-  lm(bmi ~ LIPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)

# Moderate to Vigorous Physical activity

lm_MVPA3_presc <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MVPA3_child <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MVPA3_adoles <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MVPA3_younger <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MVPA3_middle <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MVPA3_older <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)
# Moderate Physical activity

lm_MPA3_presc <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_MPA3_child <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_MPA3_adoles <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_MPA3_younger <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_MPA3_middle <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_MPA3_older <-  lm(bmi ~ MPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)
# Vigorous Physical activity

lm_VPA3_presc <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                     data = presc)
lm_VPA3_child <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = child)
lm_VPA3_adoles <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = adoles)
lm_VPA3_younger <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = younger)
lm_VPA3_middle <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = middle)
lm_VPA3_older <-  lm(bmi ~ VPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                     data = older)

# Moderate to Vigorous Physical activity

lm_MVPA3_presc <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + season_cat1 + season_cat2 + season_cat3,
                      data = presc)
lm_MVPA3_child <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat1 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = child)
lm_MVPA3_adoles <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + study_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = adoles)
lm_MVPA3_younger <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = younger)
lm_MVPA3_middle <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = middle)
lm_MVPA3_older <-  lm(bmi ~ MVPA3_10_min_day + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = older)

presc_bmi <- round(c(coef(lm_SB3_presc)[2],confint(lm_SB3_presc)[2],confint(lm_SB3_presc)[13],summary(lm_SB3_presc)$coefficients[2,4],
                     coef(lm_LIPA3_presc)[2],confint(lm_LIPA3_presc)[2],confint(lm_LIPA3_presc)[13],summary(lm_LIPA3_presc)$coefficients[2,4],
                     coef(lm_MPA3_presc)[2],confint(lm_MPA3_presc)[2],confint(lm_MPA3_presc)[13],summary(lm_MPA3_presc)$coefficients[2,4],
                     coef(lm_VPA3_presc)[2],confint(lm_VPA3_presc)[2],confint(lm_VPA3_presc)[13],summary(lm_VPA3_presc)$coefficients[2,4],
                     coef(lm_MVPA3_presc)[2],confint(lm_MVPA3_presc)[2],confint(lm_MVPA3_presc)[13],summary(lm_MVPA3_presc)$coefficients[2,4]),2)

child_bmi <- round(c(coef(lm_SB3_child)[2],confint(lm_SB3_child)[2],confint(lm_SB3_child)[14],summary(lm_SB3_child)$coefficients[2,4],
                     coef(lm_LIPA3_child)[2],confint(lm_LIPA3_child)[2],confint(lm_LIPA3_child)[14],summary(lm_LIPA3_child)$coefficients[2,4],
                     coef(lm_MPA3_child)[2],confint(lm_MPA3_child)[2],confint(lm_MPA3_child)[14],summary(lm_MPA3_child)$coefficients[2,4],
                     coef(lm_VPA3_child)[2],confint(lm_VPA3_child)[2],confint(lm_VPA3_child)[14],summary(lm_VPA3_child)$coefficients[2,4],
                     coef(lm_MVPA3_child)[2],confint(lm_MVPA3_child)[2],confint(lm_MVPA3_child)[14],summary(lm_MVPA3_child)$coefficients[2,4]),2)

adoles_bmi <- round(c(coef(lm_SB3_adoles)[2],confint(lm_SB3_adoles)[2],confint(lm_SB3_adoles)[13],summary(lm_SB3_adoles)$coefficients[2,4],
                      coef(lm_LIPA3_adoles)[2],confint(lm_LIPA3_adoles)[2],confint(lm_LIPA3_adoles)[13],summary(lm_LIPA3_adoles)$coefficients[2,4],
                      coef(lm_MPA3_adoles)[2],confint(lm_MPA3_adoles)[2],confint(lm_MPA3_adoles)[13],summary(lm_MPA3_adoles)$coefficients[2,4],
                      coef(lm_VPA3_adoles)[2],confint(lm_VPA3_adoles)[2],confint(lm_VPA3_adoles)[13],summary(lm_VPA3_adoles)$coefficients[2,4],
                      coef(lm_MVPA3_adoles)[2],confint(lm_MVPA3_adoles)[2],confint(lm_MVPA3_adoles)[13],summary(lm_MVPA3_adoles)$coefficients[2,4]),2)

younger_bmi <- round(c(coef(lm_SB3_younger)[2],confint(lm_SB3_younger)[2],confint(lm_SB3_younger)[12],summary(lm_SB3_younger)$coefficients[2,4],
                       coef(lm_LIPA3_younger)[2],confint(lm_LIPA3_younger)[2],confint(lm_LIPA3_younger)[12],summary(lm_LIPA3_younger)$coefficients[2,4],
                       coef(lm_MPA3_younger)[2],confint(lm_MPA3_younger)[2],confint(lm_MPA3_younger)[12],summary(lm_MPA3_younger)$coefficients[2,4],
                       coef(lm_VPA3_younger)[2],confint(lm_VPA3_younger)[2],confint(lm_VPA3_younger)[12],summary(lm_VPA3_younger)$coefficients[2,4],
                       coef(lm_MVPA3_younger)[2],confint(lm_MVPA3_younger)[2],confint(lm_MVPA3_younger)[12],summary(lm_MVPA3_younger)$coefficients[2,4]),2)

middle_bmi <- round(c(coef(lm_SB3_middle)[2],confint(lm_SB3_middle)[2],confint(lm_SB3_middle)[12],summary(lm_SB3_middle)$coefficients[2,4],
                      coef(lm_LIPA3_middle)[2],confint(lm_LIPA3_middle)[2],confint(lm_LIPA3_middle)[12],summary(lm_LIPA3_middle)$coefficients[2,4],
                      coef(lm_MPA3_middle)[2],confint(lm_MPA3_middle)[2],confint(lm_MPA3_middle)[12],summary(lm_MPA3_middle)$coefficients[2,4],
                      coef(lm_VPA3_middle)[2],confint(lm_VPA3_middle)[2],confint(lm_VPA3_middle)[12],summary(lm_VPA3_middle)$coefficients[2,4],
                      coef(lm_MVPA3_middle)[2],confint(lm_MVPA3_middle)[2],confint(lm_MVPA3_middle)[12],summary(lm_MVPA3_middle)$coefficients[2,4]),2)

older_bmi <- round(c(coef(lm_SB3_older)[2],confint(lm_SB3_older)[2],confint(lm_SB3_older)[12],summary(lm_SB3_older)$coefficients[2,4],
                     coef(lm_LIPA3_older)[2],confint(lm_LIPA3_older)[2],confint(lm_LIPA3_older)[12],summary(lm_LIPA3_older)$coefficients[2,4],
                     coef(lm_MPA3_older)[2],confint(lm_MPA3_older)[2],confint(lm_MPA3_older)[12],summary(lm_MPA3_older)$coefficients[2,4],
                     coef(lm_VPA3_older)[2],confint(lm_VPA3_older)[2],confint(lm_VPA3_older)[12],summary(lm_VPA3_older)$coefficients[2,4],
                     coef(lm_MVPA3_older)[2],confint(lm_MVPA3_older)[2],confint(lm_MVPA3_older)[12],summary(lm_MVPA3_older)$coefficients[2,4]),2)

sex_age <- data.frame(rbind(presc_bmi, child_bmi, adoles_bmi, younger_bmi,middle_bmi, older_bmi), row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age) <- c("SB3","2.5%", "97.5%","p.value", "LIPA3","2.5%.1", "97.5%.1", "p.value1", "MPA3","2.5%.2", "97.5%.2", "p.value2", "VPA3","2.5%.3", "97.5%.3", "p.value3", "MVPA3","2.5%.4", "97.5%.4", "p.value4")

group1 <- sprintf("%s (%s, %s) p = %s", sex_age$SB3, sex_age$`2.5%`, sex_age$`97.5%`, sex_age$p.value)
group2 <- sprintf("%s (%s, %s) p = %s", sex_age$LIPA3, sex_age$`2.5%.1`, sex_age$`97.5%.1`, sex_age$p.value1)
group3 <- sprintf("%s (%s, %s) p = %s", sex_age$MPA3, sex_age$`2.5%.2`, sex_age$`97.5%.2`, sex_age$p.value2)
group4 <- sprintf("%s (%s, %s) p = %s", sex_age$VPA3, sex_age$`2.5%.3`, sex_age$`97.5%.3`, sex_age$p.value3)
group5 <- sprintf("%s (%s, %s) p = %s", sex_age$MVPA3, sex_age$`2.5%.4`, sex_age$`97.5%.4`, sex_age$p.value4)

sex_age2 <- data.frame(group1, group2, group3, group4, group5, row.names = c("PRE-SCHOOLERS","CHILDREN", "ADOLESCENTS","YOUNGER ADULTS", "MIDDLE ADULTS","OLDER ADULTS"))
colnames(sex_age2) <- c("SB3", "LIPA3","MPA3","VPA3","MVPA3")

write.xlsx(sex_age2, "path/cutpoint3_adjusted_15.xlsx", 
           colNames = TRUE, rowNames = FALSE, append = FALSE)