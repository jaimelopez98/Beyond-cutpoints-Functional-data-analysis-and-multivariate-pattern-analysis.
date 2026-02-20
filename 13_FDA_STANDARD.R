# Script name: 13_FDA_STANDARD.R

# Author: J.López, Inserm

# Doing: 
#     *Standardize the probabiblity denstifunction of physical activity to reduce error of PDF estimation (dividing PDF
#       by total surface under the PDF curve estimated by trapeze methods)

# PACKAGES 

library(tidyr)
library(plyr)
library(dplyr)
library(purrr)
library(pracma)
library(ggplot2)

# LOAD THE DATA (obtained in 12_ACT_KERNEL) 

load("path/dst_15.rda")

#Compute total area under the curve 
area_dst <- dst %>% 
  group_by(stno) %>% 
  summarise(surf = trapz(x, f_i))

summary(area_dst$surf) # > Some are not equal to 1

# Standardize density function by total area under the curve
dst_2 <- dst %>% 
  left_join(area_dst, by = "stno") %>% 
  mutate(f_i_2 = f_i/surf)

# Recompute total area 
area_dst_2 <- dst_2 %>% 
  group_by(stno) %>% 
  summarise(surf = trapz(x, f_i_2))

# Check if new total area = 1
summary(area_dst_2$surf)

# Save
save(dst_2, file = "path/trap_15.rda") # Save it for next code (14_FDA_DISTRIBUTION)