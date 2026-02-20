# Script name: 02_DATA_EXTRACTION.R

# Author: J.Lopez, Inserm

# Doing: Extracting time in each intensity using GGIR time-series and PA variables for Multivariate Pattern analysis.
# (2026-02-23 data release)

# PACKAGES ----

library(tidyr)
library(plyr)
library(dplyr)
library(haven)
library(stringr)
library(purrr)
library(vroom)
library(lubridate)
library(hms)

# FUNCTION ----

extraction_PA_variables <- function(data, acc_ranges) {
  results_list <- list() 
  
  for (i in seq_along(acc_ranges[-length(acc_ranges)])) {
    range_min <- acc_ranges[i]
    range_max <- acc_ranges[i + 1] - 1  
    
    result <- data %>%
      filter(ACC >= range_min & ACC <= range_max) %>%
      group_by(ID) %>%
      summarise(ACC_sum = sum(duration), .groups = "drop") %>%
      rename_with(~ paste0("ACC_", range_min, "_", range_max), ACC_sum)
    
    results_list[[length(results_list) + 1]] <- result
  }
  
  merged_results <- results_list[[1]]  
  for (i in 2:length(results_list)) {
    merged_results <- full_join(merged_results, results_list[[i]], by = c("ID"))
  }
  
  return(merged_results) 
}

# 1) LOAD ALL THE FILES OF TIME-SERIES IN THE SAME OBJECT  -----

# Path of 15sec epoch files
path.files <- "/path/"

acc_data <- data.frame(files = list.files(path = path.files,
                                          pattern = ".csv$",
                                          recursive = TRUE)) %>% 
  mutate(ID = as.numeric(str_extract(files, "[0-9]+"))) %>% 
  split(.$ID, drop = T) %>%
  map_dfr(~ {
    
    i_dat <- vroom(file = paste0(path.files, unique(.x$files)), 
                   delim = ",")
    
  }, .id = "ID") %>% 
  mutate(ID = as.integer(.$ID),
         timenum = as.POSIXct(timenum, origin="1970-1-1", tz="Europe/Paris"),
         timeserie = as.POSIXct(format(timenum, format = "%H:%M:%S"), format = "%H:%M:%S"), #create a new column with the same day for all (TIME-SERIES)
         weekday = wday(timenum), # extract the day ("Sunday" = 1, "Monday" = 2, ...) #extract the day of the week
         daytype = ifelse(wday(timenum) %in% 2:6, 0, 1), # extract the type of day (WD = 0  vs WE = 1) 
         calendar_date = as.Date(format(timenum, format = "%Y-%m-%d"),format = "%Y-%m-%d")) %>% 

# Restricting the time window
  
acc_data <- acc_data %>% filter(timeserie >= "2026-02-23 06:00:00 CET" &  timeserie <= "2026-02-23 22:00:00 CET") 

# 2) REMOVE NON WEAR TIME AND SLEEP PERIOD TIME

epoch <- 15 # change to 60s for sensitivity analysis

acc_data_valid <- acc_data %>%
  filter(SleepPeriodTime == 0,
         invalidepoch == 0) %>%  
  mutate(duration = epoch/60) #it represents the time of each epoch by a number (1 line = 1 min)

# 3) CALCULATE THE WAKING TIME OF EACH DAY BY PARTICIPANT ----

acc_ad_wak <- acc_data_valid %>%
  group_by(ID, calendar_date, daytype) %>%
  summarise(dur_day_min = sum(duration)) %>%
  ungroup()

# 4) CREATE A VARIABLE TO CONSIDER A DAY VALID OR NOT ----

acc_day <- acc_ad_wak %>%
  mutate(
    crit.wd = ifelse(daytype == 0 & dur_day_min >= 480, 1,0), # create a criteria for weekdays with more than 8h, replace to 600 for 10h
    crit.we = ifelse(daytype == 1 & dur_day_min >= 480, 1,0) # create a criteria for weekend with more than 8h, replace to 600 for 10h
  ) %>%
  filter(dur_day_min >= 480)

# 5) SUM THE NUMBER OF VALID DAY OF EACH ID AND SELECT ONLY THOSE MEETING THE STUDY PROTOCOL ----

acc_valid <- acc_day %>% 
  mutate(Nvalid = crit.wd + crit.we) %>%
  select(ID, Nvalid, crit.wd, crit.we) %>% 
  aggregate(. ~ ID, data = ., FUN = sum)%>%
  filter(crit.wd > 0 & crit.we > 0) # this criteria can change, in this case we used at least 1WD and 1WE

# 6) CREATE AN OBJECT WITH ONLY PARTICIPANT AND DAYS CONSIDERED VALIDS -----

acc_day_valid <- acc_day %>% filter(ID %in% acc_valid$ID & dur_day_min >= 480) # 8h = 480 || 10h = 600

# 7) REMOVE IN THE TIME-SERIES THE PARTICIPANTS AND DAYS CONSIDERED INVALID ----

acc_data_valid2 <- acc_data_valid %>% 
  merge(., acc_day_valid, by = c("ID", "calendar_date", "daytype"))

save(acc_data_valid2, file = "path/acc_data.rda") # Save it for next code (12_ACT_KERNEL)

# 8) CALCULATE THE TIME SPENT IN EACH INTENSITY BY DAY ----

# WAKING TIME 

acc_wak <- acc_data_valid2 %>% group_by(ID) %>%
  summarise(dur_min = sum(duration))

# SEDENTARY TIME 
acc_SB1 <- acc_data_valid2 %>% # Troiano (<26 counts/15s) for adults and Evenson (<26 counts/15s) for youth
  filter (ACC < 25) %>%
  group_by(ID) %>%
  summarise(dur_SB1 = sum(duration))

acc_SB2 <- acc_data_valid2 %>% # Matthews (<26 counts/15s) for adults and Romanzini (<47 counts/15s) for youth
  filter (ACC < 25) %>%
  group_by(ID) %>%
  summarise(dur_SB2 = sum(duration)) 

acc_SB3 <- acc_data_valid2 %>% # Swartz (<26 counts/15s) for adults and Puyau (<200 counts/15s) for youth
  filter (ACC < 25) %>%
  group_by(ID) %>%
  summarise(dur_SB3 = sum(duration))

# LIGHT PA TIME 
acc_LIPA1 <- acc_data_valid2 %>% # Troiano (26-504 counts/15s) for adults and Evenson (26-573 counts/15s) for youth
  filter (ACC >= 26 & ACC < 505) %>%
  group_by(ID) %>%
  summarise(dur_LIPA1 = sum(duration)) 

acc_LIPA2 <- acc_data_valid2 %>% # Matthews (26-189 counts/15s) for adults and Romanzini (47-606 counts/15s) for youth
  filter (ACC >= 26 & ACC < 190) %>%
  group_by(ID) %>%
  summarise(dur_LIPA2 = sum(duration))

acc_LIPA3 <- acc_data_valid2 %>% # Swartz (26-144 counts/15s) for adults and Puyau (200-799 counts/15s) for youth
  filter (ACC >= 26 & ACC < 145) %>%
  group_by(ID) %>%
  summarise(dur_LIPA3 = sum(duration))

# MODERATE PA TIME
acc_MPA1 <- acc_data_valid2 %>% # Troiano (504-1499 counts/15s) for adults and Evenson (574-1002 counts/15s) for youth
  filter (ACC >= 505 & ACC < 1500) %>%
  group_by(ID) %>%
  summarise(dur_MPA1 = sum(duration))

acc_MPA2 <- acc_data_valid2 %>% # Matthews (190-1430 counts/15s) for adults and Romanzini (607-817 counts/15s) for youth
  filter (ACC >= 190 & ACC < 1431) %>%
  group_by(ID) %>%
  summarise(dur_MPA2 = sum(duration))

acc_MPA3 <- acc_data_valid2 %>% # Swartz (145-1234 counts/15s) for adults and Puyau (800-2049 counts/15s) for youth
  filter (ACC >= 145 & ACC < 1235) %>%
  group_by(ID) %>%
  summarise(dur_MPA3 = sum(duration))

# VIGOROUS PA TIME 
acc_VPA1 <- acc_data_valid2 %>% # Troiano (>1500 counts/15s) for adults and Evenson (>1003 counts/15s) for youth
  filter (ACC >= 1500) %>%
  group_by(ID) %>%
  summarise(dur_VPA1 = sum(duration))

acc_VPA2 <- acc_data_valid2 %>% # Matthews (>1431 counts/15s) for adults and Romanzini (>818 counts/15s) for youth
  filter (ACC >= 1431) %>%
  group_by(ID) %>%
  summarise(dur_VPA2 = sum(duration))

acc_VPA3 <- acc_data_valid2 %>% # Swartz (>1235 counts/15s) for adults and Puyau (>2050 counts/15s) for youth
  filter (ACC >= 1235) %>%
  group_by(ID) %>%
  summarise(dur_VPA3 = sum(duration))

# MODERATE TO VIGOROUS PA TIME
acc_MVPA1 <- acc_data_valid2 %>% # Troiano (>505 counts/15s) for adults and Evenson (>574 counts/15s) for youth
  filter (ACC >= 505) %>%
  group_by(ID) %>%
  summarise(dur_MVPA1 = sum(duration))

acc_MVPA2 <- acc_data_valid2 %>% # Matthews (>190 counts/15s) for adults and Romanzini (>607 counts/15s) for youth
  filter (ACC >= 190) %>%
  group_by(ID) %>%
  summarise(dur_MVPA2 = sum(duration))

acc_MVPA3 <- acc_data_valid2 %>% # Swartz (>145 counts/15s) for adults and Puyau (>800 counts/15s) for youth
  filter (ACC >= 145) %>%
  group_by(ID) %>%
  summarise(dur_MVPA3 = sum(duration))

# MERGE ALL THE TIMES IN A SAME OBJECT (part5_daysummary)
acc_cut <- left_join (acc_wak, acc_SB1, by = c("ID")) %>%
  left_join (., acc_SB2, by = c("ID")) %>%
  left_join (., acc_SB3, by = c("ID")) %>%
  left_join (., acc_LIPA1, by = c("ID")) %>%
  left_join (., acc_LIPA2, by = c("ID")) %>%
  left_join (., acc_LIPA3, by = c("ID")) %>%
  left_join (., acc_MPA1, by = c("ID")) %>%
  left_join (., acc_MPA2, by = c("ID")) %>%
  left_join (., acc_MPA3, by = c("ID")) %>%
  left_join (., acc_VPA1, by = c("ID")) %>%
  left_join (., acc_VPA2, by = c("ID")) %>%
  left_join (., acc_VPA3, by = c("ID")) %>%
  left_join (., acc_MVPA1, by = c("ID")) %>%
  left_join (., acc_MVPA2, by = c("ID")) %>%
  left_join (., acc_MVPA3, by = c("ID"))
acc_cut[is.na(acc_cut)] <- 0

acc_day_cut <- left_join(acc_valid,acc_cut, by = "ID") %>%
  mutate(wear_min_day = dur_min/Nvalid,
         SB1_min_day = dur_SB1/Nvalid,
         SB2_min_day = dur_SB2/Nvalid,
         SB3_min_day = dur_SB3/Nvalid,
         LIPA1_min_day = dur_LIPA1/Nvalid,
         LIPA2_min_day = dur_LIPA2/Nvalid,
         LIPA3_min_day = dur_LIPA3/Nvalid,
         MPA1_min_day = dur_MPA1/Nvalid,
         MPA2_min_day = dur_MPA2/Nvalid,
         MPA3_min_day = dur_MPA3/Nvalid,
         VPA1_min_day = dur_VPA1/Nvalid,
         VPA2_min_day = dur_VPA2/Nvalid,
         VPA3_min_day = dur_VPA3/Nvalid,
         MVPA1_min_day = dur_MVPA1/Nvalid,
         MVPA2_min_day = dur_MVPA2/Nvalid,
         MVPA3_min_day = dur_MVPA3/Nvalid) %>%
  select(ID, Nvalid, wear_min_day, SB1_min_day,SB2_min_day,SB3_min_day, 
         LIPA1_min_day, LIPA2_min_day, LIPA3_min_day,
         MPA1_min_day, MPA2_min_day, MPA3_min_day,
         VPA1_min_day, VPA2_min_day, VPA3_min_day,
         MVPA1_min_day, MVPA2_min_day, MVPA3_min_day)

# 9) PA VARIABLES FOR MULTIVARIATE PATTERN ANALYSIS ----

acc_ranges <- c( 0, 1, 4, 7, 13, 19, 25, 32, 38, 50, 75, 100, 125, 200, 250, 375, 500, 625, 750, 875, 1000, 1125, 1250, 1375, 1500, 1750, 2000)

acc_PA_var <- extraction_PA_variables(acc_data_valid2, acc_ranges)
acc_PA_var[is.na(acc_PA_var)] <- 0

acc_day_PA_var <- left_join(acc_valid,acc_PA_var, by = "ID") %>%
  mutate(across(starts_with("ACC"), ~./Nvalid)) %>%
  select(-crit.wd, -crit.we)

acc_day_sum <- left_join (acc_day_cut, acc_day_PA_var, by = c("ID", "Nvalid"))

write.csv(acc_day_sum, file = "path/acc_day_sum.csv", row.names = FALSE)

# 10) INCLUDE SEASON (first day) AND DEMOGRAPHIC DATA ----

season <- acc_day %>% select(ID, calendar_date) %>%
  aggregate(calendar_date ~ ID, data = ., FUN = function(x) x[1]) %>%
  mutate(season =  ifelse((month(calendar_date) == 3 & day(calendar_date) >= 21) | month(calendar_date) %in% c(4, 5) | (month(calendar_date) == 6 & day(calendar_date) < 21), 1,
                          ifelse((month(calendar_date) == 6 & day(calendar_date) >= 21) | month(calendar_date) %in% c(7, 8) | (month(calendar_date) == 9 & day(calendar_date) < 21), 2,
                                 ifelse((month(calendar_date) == 9 & day(calendar_date) >= 21) | month(calendar_date) %in% c(10, 11) | (month(calendar_date) == 12 & day(calendar_date) < 21), 3, 0)))) %>%
  select(-calendar_date)

demo_data <- read.csv("path/demo_data.csv")

norway_data <- merge(demo_data, season, by="ID") %>%
  merge(., acc_day_sum, by = "ID") %>%
  select(-X, -Nvalid ) %>%
  filter(complete.cases(.))

write.csv(norway_data, file = "path/data.csv", row.names = FALSE)
