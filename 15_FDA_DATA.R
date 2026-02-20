# Script name: 15_FDA_data_WD.R

# Author: J.López, Inserm

# Doing: 
#   * Preparing data for function-on-scalar regression in physical activity over the full range of the activity intensity distribution for each age group

# PACKAGES ----

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)

# DATA ----
load("path/act_15.rda")

data <- read.csv("pathdata_ref.csv")%>% 
  arrange(stno) %>%
  mutate(wear_kernel = wear_min_day-ACC_0_0)

# FULL SAMPLE (n= 7092) ----

tab_pa_full <- act %>%
  filter(stno %in% unique(data$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-stno,-gender, -age, -age_cat,-season,-edu)
  
stno <- as.vector(data$stno)
dimnames(tab_pa_full) <- list(stno, dimnames(tab_pa_full)[[2]])
Y_Activity <- I(as.matrix(tab_pa_full)) 

tab_cov <- data %>%
  select(stno, bmi,bmi_cat, wear_kernel, wear_min_day, gender, age, age1,age2, edu, edu_cat1, edu_cat2, season, season_cat1, season_cat2,season_cat3, study, study_cat1,study_cat2,study_cat3, ACC_0_0)

data_full <- tab_cov %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_full,  file = "path/data_full.rda")

# YOUTH (n=1757) ----
data_youth <- data %>% filter(age<19) 
act_youth <- act %>% filter(age<19)

tab_pa_youth <- act_youth %>%
  filter(stno %in% unique(data_youth$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_youth$stno)
dimnames(tab_pa_youth) <- list(stno, dimnames(tab_pa_youth)[[2]])
Y_Activity <- I(as.matrix(tab_pa_youth)) 

tab_cov_youth <- tab_cov %>%  filter(age<19)

data_youth <- tab_cov_youth %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_youth,  file = "path/data_youth.rda")

# ADULTHOOD (n=1757) ----
data_adulthood <- data %>% filter(age>=19) 
act_adulthood <- act %>% filter(age>=19)

tab_pa_adulthood <- act_adulthood %>%
  filter(stno %in% unique(data_adulthood$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
  select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_adulthood$stno)
dimnames(tab_pa_adulthood) <- list(stno, dimnames(tab_pa_adulthood)[[2]])
Y_Activity <- I(as.matrix(tab_pa_adulthood)) 

tab_cov_adulthood <- tab_cov %>%  filter(age>=19)

data_adulthood <- tab_cov_adulthood %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_adulthood,  file = "path/data_adulthood.rda")

# PRESCHOOLERS (n=1137) ----
data_presc <- data %>% filter(age<6) 
act_presc <- act %>% filter(age<6)

tab_pa_presc <- act_presc %>%
  filter(stno %in% unique(data_presc$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_presc$stno)
dimnames(tab_pa_presc) <- list(stno, dimnames(tab_pa_presc)[[2]])
Y_Activity <- I(as.matrix(tab_pa_presc)) 

tab_cov_presc <- tab_cov %>%  filter(age<6)

data_presc <- tab_cov_presc %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_presc,  file = "path/data_presc.rda")

# CHILDREN (n=1246) ----
data_child <- data %>% filter(age >=6 & age <11) 
act_child <- act %>% filter(age >=6 & age <11)

tab_pa_child <- act_child %>%
  filter(stno %in% unique(data_child$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_child$stno)
dimnames(tab_pa_child) <- list(stno, dimnames(tab_pa_child)[[2]])
Y_Activity <- I(as.matrix(tab_pa_child)) 

tab_cov_child <- tab_cov %>%  filter(age >=6 & age <11)

data_child <- tab_cov_child %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_child,  file = "path/data_child.rda")

# ADOLESCENTS (n=454) ----
data_adoles <- data %>% filter(age >=11 & age <19) 
act_adoles <- act %>% filter(age >=11 & age <19)

tab_pa_adoles <- act_adoles %>%
  filter(stno %in% unique(data_adoles$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_adoles$stno)
dimnames(tab_pa_adoles) <- list(stno, dimnames(tab_pa_adoles)[[2]])
Y_Activity <- I(as.matrix(tab_pa_adoles)) 

tab_cov_adoles <- tab_cov %>%  filter(age >=11 & age <19)

data_adoles <- tab_cov_adoles %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_adoles,  file = "path/data_adoles.rda")

# YOUNGER ADULTS (n=1447) ----
data_younger <- data %>% filter(age >=19 & age <45) 
act_younger <- act %>% filter(age >=19 & age <45)

tab_pa_younger <- act_younger %>%
  filter(stno %in% unique(data_younger$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_younger$stno)
dimnames(tab_pa_younger) <- list(stno, dimnames(tab_pa_younger)[[2]])
Y_Activity <- I(as.matrix(tab_pa_younger)) 

tab_cov_younger <- tab_cov %>%  filter(age >=19 & age <45)

data_younger <- tab_cov_younger %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_younger,  file = "path/data_younger.rda")

# MIDDLE ADULTS (n=1757) ----
data_middle <- data %>% filter(age >=45 & age <65) 
act_middle <- act %>% filter(age >=45 & age <65)

tab_pa_middle <- act_middle %>%
  filter(stno %in% unique(data_middle$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_middle$stno)
dimnames(tab_pa_middle) <- list(stno, dimnames(tab_pa_middle)[[2]])
Y_Activity <- I(as.matrix(tab_pa_middle)) 

tab_cov_middle <- tab_cov %>%  filter(age >=45 & age <65)

data_middle <- tab_cov_middle %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_middle,  file = "path/data_middle.rda")

# OLDER ADULTS (n=1051) ----
data_older <- data %>% filter(age >=65) 
act_older <- act %>% filter(age >=65)

tab_pa_older <- act_older %>%
  filter(stno %in% unique(data_older$stno)) %>% 
  spread(key = x, value = A_i) %>% 
  arrange(stno) %>%
    select(-stno,-gender, -age, -age_cat,-season,-edu)

stno <- as.vector(data_older$stno)
dimnames(tab_pa_older) <- list(stno, dimnames(tab_pa_older)[[2]])
Y_Activity <- I(as.matrix(tab_pa_older)) 

tab_cov_older <- tab_cov %>%  filter(age >=65)

data_older <- tab_cov_older %>%
  cbind(Y_Activity) %>%
  mutate(stno = as.factor(.$stno)) 

save(data_older,  file = "path/data_older.rda")