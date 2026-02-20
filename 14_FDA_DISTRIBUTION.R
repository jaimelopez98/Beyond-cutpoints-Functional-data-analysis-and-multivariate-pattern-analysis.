# Script name: 14_FDA_DISTRIBUTON.R

# Author: J.López, Inserm

# Doing: 
#     *Computing activity distribution from mean daily waking time and density function for each participant.

# PACKAGES ----
library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(stringr)
library(haven)
library(ggplot2)

# 0) LOAD  PA AND DEMOGRAPHIC DATA  ----
load("path/trap_15.rda")

data <- read.csv("path/fdata_ref.csv") %>%
  arrange(stno) %>%
  mutate(wear_kernel = wear_min_day-ACC_0_0)

dst_2 <- dst_2 %>%
  select(-f_i, -surf)

act <- dst_2 %>% 
  mutate(stno = as.numeric(as.character(stno))) %>%
  merge(data, by = "stno") %>% 
  mutate(A_i = f_i_2*wear_kernel)%>%
  select(stno, x, A_i, gender,age_cat,age, season,edu)

save(act, file = "path/act_15.rda") # Save it for next code (15_FDA_DATA)
