
# Script name: SA_FDA_AREA.R

# Author: J.Lopez, Inserm

# Doing: Additional analysis estimating the difference in BMI associated with a shift of 10 minutes from the time spent above this acceleration 
# the time spent in non-null acceleration below these values.
# (2026-02-23 data release)

library(dplyr)
library(tidyr)
library(tidyfun)

# PARAMETERS
s_grid <- 1:2340
ds <- diff(s_grid)[1]

# CHILDREN ----
# DATA

load("path/act_15.rda")

load("path/models/data_child.rda")

load("path/coefficients/result_beta_child.rda")

act_child <- act %>% filter(age >=6 & age <11)

pa_child <- act_child %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_child_region1 <- pa_child[,c(1,174:475)]
pa_child_region1 <- pa_child_region1 %>% 
  mutate(region1 = rowSums(across(-1))) %>%
  select(stno, region1)

pa_child_region2 <- pa_child[,c(1,926:1918)]
pa_child_region2 <- pa_child_region2 %>% 
  mutate(region2 = rowSums(across(-1))) %>%
  select(stno, region2)

pa_child_regions <- merge(pa_child_region1,pa_child_region2, by= "stno")

data_child <- merge(data_child,pa_child_regions, by= "stno")

data_child_region <- data_child %>%
  select(stno, bmi, region1, region2,wear_kernel, wear_min_day, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# AREA UNDER THE CURVE (beta coefficients)
# Region 1 (positive area between 173 and 474)

idxB_child1 <- which(s_grid >=173 & s_grid <= 474)
A_B_child1 <- sum(result_beta_child[idxB_child1,2]*ds)

lm_region1_child <-  lm(bmi ~ region1 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 + study_cat1 + study_cat2,
                    data = data_child_region)
summary(lm_region1_child)

A_B_child1/(474-173)
summary(lm_region1_child)$coefficients[2,1]*10

# Region 2 (negative area between 925 and 2340)

idxB_child2  <- which(s_grid >=925 & s_grid <=1917)
A_B_child2 <- sum(result_beta_child[idxB_child2 ,2]*ds)

lm_region2_child <- lm(bmi ~ region2 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 + study_cat1 + study_cat2,
                        data = data_child_region)

summary(lm_region2_child)

A_B_child2/(1917-925)
summary(lm_region2_child)$coefficients[2,1]*10

# ADOLESCENTS ----
# DATA

load("path/act_15.rda")

load("path/data_adoles.rda")

load("path/result_beta_adoles.rda")

act_adoles <- act %>% filter(age >=11 & age <18)

pa_adoles <- act_adoles %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_adoles_region1 <- pa_adoles[,c(1,1351:2064)]
pa_adoles_region1 <- pa_adoles_region1 %>% 
  mutate(region1 = rowSums(across(-1))) %>%
  select(stno, region1)

data_adoles <- merge(data_adoles,pa_adoles_region1, by= "stno")

data_adoles_region <- data_adoles %>%
  select(stno, bmi, region1, wear_min_day, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# AREA UNDER THE CURVE (beta coefficients)
# Region 1 (positive area between 1350 and 2063)

idxB_adoles1 <- which(s_grid >=1350 & s_grid <= 2063)
A_B_adoles1 <- sum(result_beta_adoles[idxB_adoles1,2]*ds)

lm_region1_adoles <-  lm(bmi ~ region1 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                          data = data_adoles_region)
summary(lm_region1_adoles)

A_B_adoles1/(2063-1350)
summary(lm_region1_adoles)$coefficients[2,1]*10

# YOUNGER ADULTS ----
# DATA

load("path/act_15.rda")

load("path/data_younger.rda")

load("path/result_beta_younger.rda")

act_younger <- act %>% filter(age >=19 & age <45)

pa_younger <- act_younger %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_younger_region1 <- pa_younger[,c(1,2:111)]
pa_younger_region1 <- pa_younger_region1 %>% 
  mutate(region1 = rowSums(across(-1))) %>%
  select(stno, region1)

pa_younger_region2 <- pa_younger[,c(1,966:2182)]
pa_younger_region2 <- pa_younger_region2 %>% 
  mutate(region2 = rowSums(across(-1))) %>%
  select(stno, region2)

pa_younger_regions <- merge(pa_younger_region1,pa_younger_region2, by= "stno")

data_younger <- merge(data_younger,pa_younger_regions, by= "stno")

data_younger_region <- data_younger %>%
  select(stno, bmi, region1, region2, wear_min_day, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# AREA UNDER THE CURVE (beta coefficients)
# Region 1 (positive area between 1 and 110)

idxB_younger1 <- which(s_grid >=1 & s_grid <= 110)
A_B_younger1 <- sum(result_beta_younger[idxB_younger1,2]*ds)

lm_region1_younger <-  lm(bmi ~ region1 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = data_younger_region)
summary(lm_region1_younger)

A_B_younger1/(110-1)
summary(lm_region1_younger)$coefficients[2,1]*10

# Region 2 (negative area between 965 and 2181)

idxB_younger2  <- which(s_grid >= 965 & s_grid <=2181)
A_B_younger2 <- sum(result_beta_younger[idxB_younger2 ,2]*ds)

lm_region2_younger <- lm(bmi ~ region2 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                       data = data_younger_region)

summary(lm_region2_younger)

A_B_younger2/(2181-965)
summary(lm_region2_younger)$coefficients[2,1] *10

# MIDDLE ADULTS ----
# DATA

load("path/act_15.rda")

load("path/data_middle.rda")

load("path/result_beta_middle.rda")

act_middle <- act %>% filter(age >=45 & age <65)

pa_middle <- act_middle %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_middle_region1 <- pa_middle[,c(1,2:123)]
pa_middle_region1 <- pa_middle_region1 %>% 
  mutate(region1 = rowSums(across(-1))) %>%
  select(stno, region1)

pa_middle_region2 <- pa_middle[,c(1,836:2255)]
pa_middle_region2 <- pa_middle_region2 %>% 
  mutate(region2 = rowSums(across(-1))) %>%
  select(stno, region2)

pa_middle_regions <- merge(pa_middle_region1,pa_middle_region2, by= "stno")

data_middle <- merge(data_middle,pa_middle_regions, by= "stno")

data_middle_region <- data_middle %>%
  select(stno, bmi, region1, region2, wear_min_day, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# AREA UNDER THE CURVE (beta coefficients)
# Region 1 (positive area between 1 and 122)

idxB_middle1 <- which(s_grid >=1 & s_grid <= 122)
A_B_middle1 <- sum(result_beta_middle[idxB_middle1,2]*ds)

lm_region1_middle <-  lm(bmi ~ region1 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                          data = data_middle_region)
summary(lm_region1_middle)

A_B_middle1/(122-1)
summary(lm_region1_middle)$coefficients[2,1]*10

# Region 2 (negative area between 835 and 2254)

idxB_middle2  <- which(s_grid >= 835 & s_grid <=2254)
A_B_middle2 <- sum(result_beta_middle[idxB_middle2 ,2]*ds)

lm_region2_middle <- lm(bmi ~ region2 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                         data = data_middle_region)

summary(lm_region2_middle)

A_B_middle2/(2254-835)
summary(lm_region2_middle)$coefficients[2,1]*10

# OLDER ADULTS ----
# DATA

load("path/act_15.rda")

load("path/data_older.rda")

load("path/result_beta_older.rda")

act_older <- act %>% filter(age >=65)

pa_older <- act_older %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_older_region1 <- pa_older[,c(1,2:115)]
pa_older_region1 <- pa_older_region1 %>% 
  mutate(region1 = rowSums(across(-1))) %>%
  select(stno, region1)

pa_older_region2 <- pa_older[,c(1,748:1772)]
pa_older_region2 <- pa_older_region2 %>% 
  mutate(region2 = rowSums(across(-1))) %>%
  select(stno, region2)

pa_older_regions <- merge(pa_older_region1,pa_older_region2, by= "stno")

data_older <- merge(data_older,pa_older_regions, by= "stno")

data_older_region <- data_older %>%
  select(stno, bmi, region1, region2, wear_min_day, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# AREA UNDER THE CURVE (beta coefficients)
# Region 1 (positive area between 1 and 114)

idxB_older1 <- which(s_grid >=1 & s_grid <= 114)
A_B_older1 <- sum(result_beta_older[idxB_older1,2]*ds)

lm_region1_older <-  lm(bmi ~ region1 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                         data = data_older_region)
summary(lm_region1_older)

A_B_older1/(114-1)
summary(lm_region1_older)$coefficients[2,1]*10

# Region 2 (negative area between 747 and 1771)

idxB_older2  <- which(s_grid >= 747 & s_grid <=1771)
A_B_older2 <- sum(result_beta_older[idxB_older2 ,2]*ds)

lm_region2_older <- lm(bmi ~ region2 + wear_min_day + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                        data = data_older_region)

summary(lm_region2_older)

A_B_older2/(1771-747)
summary(lm_region2_older)$coefficients[2,1]*10

