# Script name: 12_FDA_KERNEL.R

# Author: J.López, Inserm

# Doing: 
#   *Estimating the probabiblity denstity function for each participant

# PACKAGES ----
library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(ks)
library(pscl)
library(lubridate)
library(hms)
library(ggplot2)
                            
# 0) LOAD TIME-SERIES AND DEMOGRAPHIC CHARACTERISTICS ----

load("path/acc_data.rda")
labda_data <- read.csv("path/data_ref.csv")

v_max <- all_acc %>%
  group_by(stno) %>%
  summarise(v_99 = quantile(ACC, 0.99))

v_95 <- quantile(v_max$v_99,0.95)

data<- all_acc %>% filter(.$stno %in% labda_data$stno) %>% filter(ACC>0)

pts <- seq(1:2000)

# Estimate median bandwidth
h.med <- data %>% 
  split(.$stno) %>%
  # Extract data from the large dataset
  map_dbl(~ {
    
    acc <- .x$ACC
    
    # Density function estimation for the participant
    fit <- kde(acc,
               eval.points = pts, # at given points
               density = TRUE) # only positive data
    
    # Get bandwidth value
    fit$h
    
  }) %>%
  # Median bandwidth
  median(.)

# Estimate individual density function with a commun bandwidth
dst <- data %>% 
  # Keep only acceleration during waking time and valid days (needs to be implemented)
  
  split(.$stno) %>% 
  map_dfr(~ {
    
    acc <- .x$ACC
    
    # Density function estimation for the participant
    fit <- kde(acc, 
               eval.points = pts, # at given coordinates
               h = h.med, # with a given bandwidth 
               density = TRUE) # only positive data
    
    # > Continuous density function for positive values
    data.frame(x = fit$eval.points, # coordinates at which the function was estimated
               f_i = fit$estimate) # estimated function
    
  }, .id = "stno") %>%
  mutate(stno = as.numeric(.$stno))

any(dst$f_i < 0) # Check if there is no negative values

save(dst, file = "path/dst_15.rda") # Save it for next code (13_FDA_STANDARD)
