# Script name: 03_COVARIATES.R

# Author: J.Lopez, Inserm

# Doing: Creation of categorical covariates needed for further analysis.
# (2026-02-23 data release)

# PACKAGES ----

library(dplyr)

categorical = function(x){
  # Author: Ian Danilevicz 
  # Goal: this function transform a vector in a dummy matrix
  # Purpose: this function is useful to transform categorical age in a handle matrix 
  p = length(table(x))
  n = length(x)
  B = matrix(0, ncol = (p-1),nrow = n)
  for(j in 1:(p-1)){
    B[,j] = ifelse(x==j, 1, 0)
  }
  x = as.factor(x)
  colnames(B) = levels(x)[-p]
  return(B)
}

data <- read.csv("path/data.csv")

age_cat <- categorical(data$age_cat)
edu_cat <- categorical(data$edu)
season_cat <- categorical(data$season)
study_cat <- categorical(data$study)

data$age1 <- age_cat[,1]
data$age2 <- age_cat[,2]

data$edu_cat1 <- edu_cat[,1]
data$edu_cat2 <- edu_cat[,2]

data$season_cat1 <- season_cat[,1]
data$season_cat2 <- season_cat[,2]
data$season_cat3 <- season_cat[,3]

data$study_cat1 <- study_cat[,1]
data$study_cat2 <- study_cat[,2]
data$study_cat3 <- study_cat[,3]

youth_data <- data %>% filter(age<18)
adulthood_data <- data %>% filter(age>18)

write.csv(data, file = "path/data_ref.csv", row.names = FALSE)
write.csv(youth_data, file = "path/youth_data_ref.csv", row.names = FALSE)
write.csv(adulthood_data, file = "path/adulthood_data_ref.csv", row.names = FALSE)