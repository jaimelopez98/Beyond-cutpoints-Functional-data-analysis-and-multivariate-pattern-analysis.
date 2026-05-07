
# Script name: SA_FDA_AREA.R

# Author: J.Lopez, Inserm

# Doing: Additional analysis estimating the difference in BMI associated with a shift of 10 minutes from the time spent above this acceleration 
# the time spent in non-null acceleration below these values.
# (2026-02-23 data release)

library(dplyr)
library(tidyr)
library(tidyfun)

# PARAMETERS
s_grid <- 1:2000
ds <- diff(s_grid)[1]

# ACT DATA

load("path/act_15.rda")

# LINEAR APPROACH ----

# CHILDREN ----
# DATA

load("path/data_child.rda")

load("path/beta_child.rda")

act_child <- act %>% filter(age >=6 & age <11)

pa_child <- act_child %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_child_region <- pa_child[,c(1,919:2001)]
pa_child_region <- pa_child_region %>% 
  mutate(region = rowSums(across(-1))) %>%
  select(stno, region)

data_child <- merge(data_child,pa_child_region, by= "stno") %>% filter(stno != 465 & stno != 666 & stno != 669)

data_child_region <- data_child %>%
  select(stno, bmi, region, wear_kernel, ACC_0_0, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# LINEAR MODEL FOR A SPECIFIC REGION
# Region (negative area between 919 and 2000)

lm_region_child <- lm(bmi ~ region + wear_kernel + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 + study_cat1 + study_cat2,
                        data = data_child_region)

summary(lm_region_child)
delta_child <- summary(lm_region_child)$coefficients[2,1]*10
lower_child <- confint(lm_region_child)[2,1]*10
upper_child <- confint(lm_region_child)[2,2]*10

child_change <- data.frame(
  Age_group = "Children (6-10y)",
  Delta = round(delta_child,2),
  Upper = round(upper_child,2),
  Lower = round(lower_child,2)
)

# ADOLESCENTS ----
# DATA

load("path/data_adoles.rda")

load("path/beta_adoles.rda")

act_adoles <- act %>% filter(age >=11 & age <18)

pa_adoles <- act_adoles %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_adoles_region <- pa_adoles[,c(1,1362:2001)]
pa_adoles_region <- pa_adoles_region %>% 
  mutate(region = rowSums(across(-1))) %>%
  select(stno, region)

data_adoles <- merge(data_adoles,pa_adoles_region, by= "stno") %>% filter (stno != 1643)

data_adoles_region <- data_adoles %>%
  select(stno, bmi, region, wear_kernel, ACC_0_0, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3, study_cat1, study_cat2)

# LINEAR MODEL FOR A SPECIFIC REGION
# Region (negative area between 1381 and 2000)

lm_region_adoles <- lm(bmi ~ region + wear_kernel + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = data_adoles_region)

summary(lm_region_adoles)
delta_adoles <- summary(lm_region_adoles)$coefficients[2,1]*10
lower_adoles <- confint(lm_region_adoles)[2,1]*10
upper_adoles <- confint(lm_region_adoles)[2,2]*10

adoles_change <- data.frame(
  Age_group = "Adolescents (11-17y)",
  Delta = round(delta_adoles,2),
  Upper = round(upper_adoles,2),
  Lower = round(lower_adoles,2)
)

# YOUNGER ADULTS ----
# DATA

load("path/data_younger.rda")

load("path/beta_younger.rda")

act_younger <- act %>% filter(age >=19 & age <45)

pa_younger <- act_younger %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_younger_region <- pa_younger[,c(1,972:2001)]
pa_younger_region <- pa_younger_region %>% 
  mutate(region = rowSums(across(-1))) %>%
  select(stno, region)


data_younger <- merge(data_younger,pa_younger_region, by= "stno")

data_younger_region <- data_younger %>%
  select(stno, bmi, region, wear_kernel, ACC_0_0, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3)

# LINEAR MODEL FOR A SPECIFIC REGION
# Region (negative area between 971 and 2000)

lm_region_younger <- lm(bmi ~ region + wear_kernel + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = data_younger_region)

summary(lm_region_younger)
delta_younger <- summary(lm_region_younger)$coefficients[2,1]*10
lower_younger <- confint(lm_region_younger)[2,1]*10
upper_younger <- confint(lm_region_younger)[2,2]*10

younger_change <- data.frame(
  Age_group = "Younger adults (45-64y)",
  Delta = round(delta_younger,2),
  Upper = round(upper_younger,2),
  Lower = round(lower_younger,2)
)

# MIDDLE ADULTS ----
# DATA

load("path/data_middle.rda")

load("path/beta_middle.rda")

act_middle <- act %>% filter(age >=45 & age <65)

pa_middle <- act_middle %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_middle_region <- pa_middle[,c(1,836:2001)]
pa_middle_region <- pa_middle_region %>% 
  mutate(region = rowSums(across(-1))) %>%
  select(stno, region)

data_middle <- merge(data_middle,pa_middle_region, by= "stno")

data_middle_region <- data_middle %>%
  select(stno, bmi, region, wear_kernel, ACC_0_0, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3)

# LINEAR MODEL FOR A SPECIFIC REGION
# Region (negative area between 835 and 2000)

lm_region_middle <- lm(bmi ~ region + wear_kernel + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = data_middle_region)

summary(lm_region_middle)
delta_middle <- summary(lm_region_middle)$coefficients[2,1]*10
lower_middle <- confint(lm_region_middle)[2,1]*10
upper_middle <- confint(lm_region_middle)[2,2]*10

middle_change <- data.frame(
  Age_group = "Middle adults (45-64y)",
  Delta = round(delta_middle,2),
  Upper = round(upper_middle,2),
  Lower = round(lower_middle,2)
)

# OLDER ADULTS ----
# DATA

load("path/data_older.rda")

load("path/beta_older.rda")

act_older <- act %>% filter(age >=65)

pa_older <- act_older %>%
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-gender, -age, -age_cat,-season,-edu)

pa_older_region <- pa_older[,c(1,745:2001)]
pa_older_region <- pa_older_region %>% 
  mutate(region = rowSums(across(-1))) %>%
  select(stno, region)

data_older <- merge(data_older,pa_older_region, by= "stno")

data_older_region <- data_older %>%
  select(stno, bmi, region, wear_kernel, ACC_0_0, age, gender, edu_cat1, edu_cat2, 
         study_cat1, season_cat1, season_cat2, season_cat3)

# LINEAR MODEL FOR A SPECIFIC REGION
# Region (negative area between 920 and 2000)

lm_region_older <- lm(bmi ~ region + wear_kernel + age + gender + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3,
                      data = data_older_region)

summary(lm_region_older)
delta_older <- summary(lm_region_older)$coefficients[2,1]*10
lower_older <- confint(lm_region_older)[2,1]*10
upper_older <- confint(lm_region_older)[2,2]*10

older_change <- data.frame(
  Age_group = "Older adults (65-90y)",
  Delta = round(delta_older,2),
  Upper = round(upper_older,2),
  Lower = round(lower_older,2)
)

data_change <- rbind(child_change, adoles_change, younger_change, middle_change, older_change)

write.csv(data_change, file="path/data_change_lm_15.csv", row.names = FALSE)

# FUNCTIONAL APPROACH ----
# CHILDREN (6-10y)----

load("path/beta_child.rda")
load("path/beta_matrix_child.rda")

Ngrid = 2000
a0 = 918
n = 1245
B = 1000
change = 10

beta <- beta_child$value
Y_Activity <- data_child$Y_Activity

delta_vec  <- rep(NA, n)
lower_vec  <- rep(NA, n)
upper_vec  <- rep(NA, n)

for(i in 1:n){
  
  T_i = data_child$wear_kernel[i]
  
  xtime_i = sum(Y_Activity[i, a0:Ngrid])
  ri = -1 + (xtime_i + change)/xtime_i   
  
  int_fi_a0 = xtime_i / T_i
  si = (ri * int_fi_a0) / (1 - int_fi_a0)
  
  fhat_i = c((1-si)*Y_Activity[i, 1:(a0-1)]/T_i,
             (1+ri)*Y_Activity[i, a0:Ngrid]/T_i)
  
  Ahat_i = fhat_i * T_i
  
  delta_i = sum(beta * Ahat_i) - sum(beta * Y_Activity[i, 1:Ngrid]) 
  delta_vec[i] = delta_i
  
  # Bootstrap CI
  v_delta = rep(NA, B)
  for(b in 1:B){
    v_delta[b] = sum(beta_mat_boot_child[b,1:Ngrid] * Ahat_i) -
      sum(beta_mat_boot_child[b,1:Ngrid] * Y_Activity[i,1:Ngrid])
  }
  
  lower_vec[i] = quantile(v_delta, 0.025)
  upper_vec[i] = quantile(v_delta, 0.975)
}

results_child <- data.frame(
  i = 1:n,
  delta_i = delta_vec,
  upper = upper_vec,
  lower = lower_vec
)

mean(results_child$delta_i)
mean(results_child$upper)
mean(results_child$lower)

# ADOLESCENTS (11-17y)----

load("path/beta_adoles.rda")
load("path/beta_matrix_adoles.rda")

Ngrid = 2000
a0 = 1361
n = 465
B = 1000
change = 10

beta <- beta_adoles$value
Y_Activity <- data_adoles$Y_Activity

delta_vec  <- rep(NA, n)
lower_vec  <- rep(NA, n)
upper_vec  <- rep(NA, n)

for(i in 1:n){
  
  T_i = data_adoles$wear_kernel[i]
  
  xtime_i = sum(Y_Activity[i, a0:Ngrid])
  ri = -1 + (xtime_i + change)/xtime_i   
  
  int_fi_a0 = xtime_i / T_i
  si = (ri * int_fi_a0) / (1 - int_fi_a0)
  
  fhat_i = c((1-si)*Y_Activity[i, 1:(a0-1)]/T_i,
             (1+ri)*Y_Activity[i, a0:Ngrid]/T_i)
  
  Ahat_i = fhat_i * T_i
  
  delta_i = sum(beta * Ahat_i) - sum(beta * Y_Activity[i, 1:Ngrid]) 
  delta_vec[i] = delta_i
  
  # Bootstrap CI
  v_delta = rep(NA, B)
  for(b in 1:B){
    v_delta[b] = sum(beta_mat_boot_adoles[b,1:Ngrid] * Ahat_i) -
      sum(beta_mat_boot_adoles[b,1:Ngrid] * Y_Activity[i,1:Ngrid])
  }
  
  lower_vec[i] = quantile(v_delta, 0.025)
  upper_vec[i] = quantile(v_delta, 0.975)
}

results_adoles <- data.frame(
  i = 1:n,
  delta_i = delta_vec,
  upper = upper_vec,
  lower = lower_vec
)

mean(results_adoles$delta_i)
mean(results_adoles$upper)
mean(results_adoles$lower)

# YOUNGER ADULTS (18-44y) ----

load("path/beta_younger.rda")
load("path/beta_matrix_younger.rda")

Ngrid = 2000
a0 = 972
n = 1447
B = 1000
change = 10

beta <- beta_younger$value
Y_Activity <- data_younger$Y_Activity

delta_vec  <- rep(NA, n)
lower_vec  <- rep(NA, n)
upper_vec  <- rep(NA, n)

for(i in 1:n){
  
  T_i = data_younger$wear_kernel[i]
  
  xtime_i = sum(Y_Activity[i, a0:Ngrid])
  ri = -1 + (xtime_i + change)/xtime_i   
  
  int_fi_a0 = xtime_i / T_i
  si = (ri * int_fi_a0) / (1 - int_fi_a0)
  
  fhat_i = c((1-si)*Y_Activity[i, 1:(a0-1)]/T_i,
             (1+ri)*Y_Activity[i, a0:Ngrid]/T_i)
  
  Ahat_i = fhat_i * T_i
  
  delta_i = sum(beta * Ahat_i) - sum(beta * Y_Activity[i, 1:Ngrid]) 
  delta_vec[i] = delta_i
  
  # Bootstrap CI
  v_delta = rep(NA, B)
  for(b in 1:B){
    v_delta[b] = sum(beta_mat_boot_younger[b,1:Ngrid] * Ahat_i) -
      sum(beta_mat_boot_younger[b,1:Ngrid] * Y_Activity[i,1:Ngrid])
  }
  
  lower_vec[i] = quantile(v_delta, 0.025)
  upper_vec[i] = quantile(v_delta, 0.975)
}

results_younger <- data.frame(
  i = 1:n,
  delta_i = delta_vec,
  upper = upper_vec,
  lower = lower_vec
)

mean(results_younger$delta_i)
mean(results_younger$upper)
mean(results_younger$lower)

# MIDDLE-AGE ADULTS(45-64y) ----

load("path/beta_middle.rda")
load("path/beta_matrix_middle.rda")

Ngrid = 2000
a0 = 835
n = 1756
B = 1000
change = 10

beta <- beta_middle$value
Y_Activity <- data_middle$Y_Activity

delta_vec  <- rep(NA, n)
lower_vec  <- rep(NA, n)
upper_vec  <- rep(NA, n)

for(i in 1:n){
  
  T_i = data_middle$wear_kernel[i]
  
  xtime_i = sum(Y_Activity[i, a0:Ngrid])
  ri = -1 + (xtime_i + change)/xtime_i   
  
  int_fi_a0 = xtime_i / T_i
  si = (ri * int_fi_a0) / (1 - int_fi_a0)
  
  fhat_i = c((1-si)*Y_Activity[i, 1:(a0-1)]/T_i,
             (1+ri)*Y_Activity[i, a0:Ngrid]/T_i)
  
  Ahat_i = fhat_i * T_i
  
  delta_i = sum(beta * Ahat_i) - sum(beta * Y_Activity[i, 1:Ngrid]) 
  delta_vec[i] = delta_i
  
  # Bootstrap CI
  v_delta = rep(NA, B)
  for(b in 1:B){
    v_delta[b] = sum(beta_mat_boot_middle[b,1:Ngrid] * Ahat_i) -
      sum(beta_mat_boot_middle[b,1:Ngrid] * Y_Activity[i,1:Ngrid])
  }
  
  lower_vec[i] = quantile(v_delta, 0.025)
  upper_vec[i] = quantile(v_delta, 0.975)
}

results_middle <- data.frame(
  i = 1:n,
  delta_i = delta_vec,
  upper = upper_vec,
  lower = lower_vec
)

mean(results_middle$delta_i)
mean(results_middle$upper)
mean(results_middle$lower)

# OLDER ADULTS (65-90y) ----

load("path/beta_older.rda")
load("path/beta_matrix_older.rda")

Ngrid = 2000
a0 = 744
n = 1049
B = 1000
change = 10

beta <- beta_older$value
Y_Activity <- data_older$Y_Activity

delta_vec  <- rep(NA, n)
lower_vec  <- rep(NA, n)
upper_vec  <- rep(NA, n)

for(i in 1:n){
  
  T_i = data_older$wear_kernel[i]
  
  xtime_i = sum(Y_Activity[i, a0:Ngrid])
  ri = -1 + (xtime_i + change)/xtime_i   
  
  int_fi_a0 = xtime_i / T_i
  si = (ri * int_fi_a0) / (1 - int_fi_a0)
  
  fhat_i = c((1-si)*Y_Activity[i, 1:(a0-1)]/T_i,
             (1+ri)*Y_Activity[i, a0:Ngrid]/T_i)
  
  Ahat_i = fhat_i * T_i
  
  delta_i = sum(beta * Ahat_i) - sum(beta * Y_Activity[i, 1:Ngrid]) 
  delta_vec[i] = delta_i
  
  # Bootstrap CI
  v_delta = rep(NA, B)
  for(b in 1:B){
    v_delta[b] = sum(beta_mat_boot_older[b,1:Ngrid] * Ahat_i) -
      sum(beta_mat_boot_older[b,1:Ngrid] * Y_Activity[i,1:Ngrid])
  }
  
  lower_vec[i] = quantile(v_delta, 0.025)
  upper_vec[i] = quantile(v_delta, 0.975)
}

results_older <- data.frame(
  i = 1:n,
  delta_i = delta_vec,
  upper = upper_vec,
  lower = lower_vec
)

mean(results_older$delta_i)
mean(results_older$upper)
mean(results_older$lower)

child_change <- data.frame(
  Age_group = "Children (6-10y)",
  Delta = round(mean(results_child$delta_i),2),
  Upper = round(mean(results_child$upper),2),
  Lower = round(mean(results_child$lower),2)
)

adoles_change <- data.frame(
  Age_group = "Adolescents (11-17y)",
  Delta = round(mean(results_adoles$delta_i),2),
  Upper = round(mean(results_adoles$upper),2),
  Lower = round(mean(results_adoles$lower),2)
)

younger_change <- data.frame(
  Age_group = "Younger adults (18-44y)",
  Delta = round(mean(results_younger$delta_i),2),
  Upper = round(mean(results_younger$upper),2),
  Lower = round(mean(results_younger$lower),2)
)

middle_change <- data.frame(
  Age_group = "Middle-age adults (45-64y)",
  Delta = round(mean(results_middle$delta_i),2),
  Upper = round(mean(results_middle$upper),2),
  Lower = round(mean(results_middle$lower),2)
)

older_change <- data.frame(
  Age_group = "Older adults (65-90y)",
  Delta = round(mean(results_older$delta_i),2),
  Upper = round(mean(results_older$upper),2),
  Lower = round(mean(results_older$lower),2)
)

data_change <- rbind(child_change, adoles_change, younger_change, middle_change, older_change)

write.csv(data_change, file="path/data_change_functional_15.csv", row.names = FALSE)
