# Script name: 01_DATA_processing.R

# Author: J.Lopez, Inserm

# Doing: Processing accelerometer files using GGIR package:
#   * Using 15s epoch and 60min of consecutive zero to considered as non-wear
#   * Restricted time window between 06:00 to 22:00, and removed time not considered "school period" in BFCS study.
#   * Combining two algorithm to de detect sleep window
#   * Removed time considered non-wear and as sleep, after used the combination of the two algorithms develop in GGIR software.
#   * Not including imputation data

# (2026-02-23 data release)

# PACKAGES ----

library(remotes)
library(GGIR)
library(actilifecounts)

# 1) GGIR CODE ----

    GGIR(datadir = "/path/data/",
     outputdir = "/path/ggir/",
     mode = 1:5,
     windowsizes =  c(15, 300, 3600), # 15s epoch and 60 min non-wear
     dataFormat = "actigraph_csv",
     extEpochData_timeformat = "%m/%d/%Y %H:%M:%S",
     acc.metric = "NeishabouriCount_y",
     overwrite = TRUE,
     idloc = 2, # extract ID
    
     # sleep window detection (2 algorithm)
     HASPT.algo = "NotWorn", 
     HASIB.algo = "Sadeh1994", # use this algorithm to remove the night in this devices wearing all the day
     
     do.imp = FALSE, # not include imputation
     visualreport = FALSE,
     outliers.only = FALSE,
     save_ms5rawlevels = TRUE,
     ignorenonwear = FALSE,
     HASPT.ignore.invalid = FALSE,
     save_ms5raw_without_invalid = FALSE,
     do.report = c(2,4,5))

