# Script name: 02_DATA_EXTRACTION.R

# Author: J.Lopez, Inserm

# Doing: Demographic analysis and mean(sd) of accelerometer data for each age group.
# (2026-02-23 data release)

# PACKAGES ----

library(dplyr)
library(purrr)

# FUNCTIONS ----

demographic_analysis <- function(df) {
  df_name <- deparse(substitute(df))               
  result_name <- paste0("summary_", df_name)       
  
  num_vars <- c("age", "bmi", "wear_min_day", "SB1_min_day", "SB2_min_day", "SB3_min_day","LIPA1_min_day", "LIPA2_min_day","LIPA3_min_day",
                "MPA1_min_day", "MPA2_min_day","MPA3_min_day","VPA1_min_day", "VPA2_min_day","VPA3_min_day","MVPA1_min_day", "MVPA2_min_day", "MVPA3_min_day")
  cat_vars <- c("gender", "bmi_cat", "edu")
  
  df <- df %>%
    mutate(
      gender = factor(gender, levels = c(0,1), labels = c("male", "female")),
      bmi_cat = factor(bmi_cat, levels = c(0,1,2,3), labels = c("1.Underweight","2. normal", "3. Overweight", "4.Obese")),
      edu = factor(edu, levels = c(0,1,2), labels = c("1.Low", "2.Medium", "3.High"))
    )
  
  num_summary <- df %>%
    summarise(across(all_of(num_vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd = ~sd(.x, na.rm = TRUE)),
                     .names = "{.col}-{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("variable", ".value"),
                 names_sep = "-") %>%
    mutate(across(c(mean, sd), ~round(.x, 1)))   
  
  cat_summary <- map_dfr(cat_vars, function(var) {
    df %>%
      count(!!sym(var)) %>%
      mutate(
        variable = paste0(var, "_", !!sym(var)),
        mean = n,
        sd = round((n / sum(n))*100, 1)       
      ) %>%
      select(variable, mean, sd)
  })
  
  final_summary <- bind_rows(num_summary, cat_summary)
  
  assign(result_name, final_summary, envir = .GlobalEnv)
}

# LOADING DATA ----

data <- read.csv("path/data_ref.csv")

presc_data <- data %>% filter(age<6)
child_data <- data %>% filter(age>=6 & age<11)
adoles_data <- data %>% filter(age>=11 & age<19)
younger_data <- data %>% filter(age>=19 & age<45)
middle_data <- data %>% filter(age>=45 & age<65)
older_data <- data %>% filter(age>=65)

demographic_analysis(presc_data)
demographic_analysis(child_data)
demographic_analysis(adoles_data)
demographic_analysis(younger_data)
demographic_analysis(middle_data)
demographic_analysis(older_data)

demo_15_full <- merge(summary_presc_data, summary_child_data, by = "variable") %>%
  merge(.,summary_adoles_data, by = "variable")%>%
  merge(.,summary_younger_data, by = "variable")%>%
  merge(.,summary_middle_data, by = "variable")%>%
  merge(.,summary_older_data, by = "variable")
  
write.csv(demo_15_full, "path/demo_15_full.csv", row.names = FALSE)

data <- read.csv("path/data_ref.csv")

presc_data <- data %>% filter(age<6)
child_data <- data %>% filter(age>=6 & age<11)
adoles_data <- data %>% filter(age>=11 & age<19)
younger_data <- data %>% filter(age>=19 & age<45)
middle_data <- data %>% filter(age>=45 & age<65)
older_data <- data %>% filter(age>=65)

demographic_analysis(presc_data)
demographic_analysis(child_data)
demographic_analysis(adoles_data)
demographic_analysis(younger_data)
demographic_analysis(middle_data)
demographic_analysis(older_data)

demo_60_full <- merge(summary_presc_data, summary_child_data, by = "variable") %>%
  merge(.,summary_adoles_data, by = "variable")%>%
  merge(.,summary_younger_data, by = "variable")%>%
  merge(.,summary_middle_data, by = "variable")%>%
  merge(.,summary_older_data, by = "variable")

write.csv(demo_60_full, "path/demo_60_full.csv", row.names = FALSE)

mean(presc_data$ACC_0_0)

mean(child_data$ACC_0_0)

mean(adoles_data$ACC_0_0)

mean(younger_data$ACC_0_0)

mean(middle_data$ACC_0_0)

mean(older_data$ACC_0_0)

