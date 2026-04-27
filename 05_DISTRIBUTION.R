# Script name: 05_DISTRIBUTION.R

# Author: J.López, Inserm

# Doing: 
#   *Plotting the activity intensity distribution for each age group using data derived functional data analysis

# PACKAGES

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# 15s EPOCH ----

plot_activity_distribution_15 <- function(data, age_col, title, cut1, cut2, cut3, lipa, mpa, vpa) {
  
  ggplot(data, aes(x = x, y = acc)) +
    geom_ribbon(aes(ymin = 0, ymax = acc),
                fill = age_col) +
    geom_line(color = age_col, size = 1) +
    geom_vline(xintercept = cut1, size = 0.7, linetype  = "dashed", col = "blue") +
    geom_vline(xintercept = cut2, size = 0.7, linetype  = "dashed", col = "blue") + 
    geom_vline(xintercept = cut3, size = 0.7, linetype  = "dashed", col = "blue") + 
    annotate("label", x = lipa,   y = 6, label = "LIPA", size = 4, family = "Times New Roman",
             label.size = 0.5, fill = "white", color = "black") + 
    annotate("label", x = mpa,   y = 6, label = "MPA", size = 4, family = "Times New Roman",
             label.size = 0.5, fill = "white", color = "black") + 
    annotate("label", x = vpa,   y = 6, label = "VPA", size = 4, family = "Times New Roman",           
             label.size = 0.5, fill = "white", color = "black") + 
    
    labs( title = title,
          y = "Time spent in each intensity (min/day)",
          x = "Intensity distribution (counts/15s)"
    ) +
    scale_x_continuous(breaks = c(1, seq(200, 2000, 200)), , expand = c(0, 0))+
    scale_y_continuous(limits = c(0,8), expand = c(0, 0))+
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      panel.grid = element_blank(), 
          plot.title = element_text(face = "bold", size = 17,hjust = 0.5),
          strip.text = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.ticks = element_line(color = "black", size = 0.8),
          axis.ticks.length = unit(0.2, "cm"),
          axis.line = element_line(color = "black")
    )
}

load("C:/Users/j_lopez/Desktop/paper2/analysis/15s/fda/data/act_15.rda") # data derived from code 14_FDA_DISTRIBUTION

data_presc <- act %>%
  filter(age<6) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=0) %>%
  filter(x<2001)
data_presc <- rbind(data.frame(x = 0, acc = 8, age = 0), data_presc)

data_child <- act %>%
  filter(age>=6 & age<11) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=1) %>%
  filter(x<2001)
data_child <- rbind(data.frame(x = 0, acc = 8, age = 1), data_child)

data_adoles <- act %>%
  filter(age>=11 & age<18) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=2) %>%
  filter(x<2001)
data_adoles <- rbind(data.frame(x = 0, acc = 8, age = 2), data_adoles)

data_younger<- act %>%
  filter(age>=18 & age<45) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=3) %>%
  filter(x<2001)
data_younger <- rbind(data.frame(x = 0, acc = 8, age = 3), data_younger)

data_middle <- act %>%
  filter(age>=45 & age<65) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=4) %>%
  filter(x<2001)
data_middle <- rbind(data.frame(x = 0, acc = 8, age = 4), data_middle)

data_older <- act %>%
  filter(age>=65) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=5) %>%
  filter(x<2001)
data_older <- rbind(data.frame(x = 0, acc = 8, age = 4), data_older)

plot_presc_15 <- plot_activity_distribution_15(data_presc, title = "Pre-schoolers (3-5y)", age_col = rgb(0.27, 0.95, 0.95, alpha = 0.5),
                                         cut1 = 25, cut2 = 573, cut3 = 1002,
                                         lipa = 300, mpa = 800, vpa = 1500)
plot_child_15 <- plot_activity_distribution_15(data_child, title = "Children (6-10y)", age_col = rgb(1, 1, 0, alpha = 0.5),
                                         cut1 = 25, cut2 = 573, cut3 = 1002,
                                         lipa = 300, mpa = 800, vpa = 1500)
plot_adoles_15 <- plot_activity_distribution_15(data_adoles, title = "Adolescents (11-18y)", age_col = rgb(0.97, 0.73, 0.08, alpha = 0.5),
                                          cut1 = 25, cut2 = 573, cut3 = 1002,
                                          lipa = 300, mpa = 800, vpa = 1500)
plot_younger_15 <- plot_activity_distribution_15(data_younger, title = "Younger adults (19-44y)", age_col = rgb(0.5, 0.2, 0.02, alpha = 0.5),
                                           cut1 = 25, cut2 = 504, cut3 = 1499,
                                           lipa = 250, mpa = 1000, vpa = 1800)
plot_middle_15 <- plot_activity_distribution_15(data_middle, title = "Middle adults (45-64y)", age_col = rgb(0.7, 0.07, 0.96, alpha = 0.5),
                                          cut1 = 25, cut2 = 504, cut3 = 1499,
                                          lipa = 250, mpa = 1000, vpa = 1800)
plot_older_15 <- plot_activity_distribution_15(data_older, title = "Older adults (65-90y)", age_col = rgb(0, 1, 0, alpha = 0.5),
                                         cut1 = 25, cut2 = 504, cut3 = 1499,
                                         lipa = 250, mpa = 1000, vpa = 1800)

(plot_presc_15 | plot_child_15 | plot_adoles_15) / 
  (plot_younger_15 | plot_middle_15 | plot_older_15)

((plot_presc_15 | plot_child_15 | plot_adoles_15) /
    plot_spacer() /
    (plot_younger_15 | plot_middle_15 | plot_older_15)) +
  plot_layout(heights = c(1, 0.01, 1))

# 60s EPOCH ----

plot_activity_distribution_60 <- function(data, age_col, title, cut1, cut2, cut3, lipa, mpa, vpa) {
  
  ggplot(data, aes(x = x, y = acc)) +
    geom_ribbon(aes(ymin = 0, ymax = acc),
                fill = age_col) +
    geom_line(color = age_col, size = 1) +
    geom_vline(xintercept = cut1, size = 0.7, linetype  = "dashed", col = "blue") +
    geom_vline(xintercept = cut2, size = 0.7, linetype  = "dashed", col = "blue") + 
    geom_vline(xintercept = cut3, size = 0.7, linetype  = "dashed", col = "blue") + 
    annotate("label", x = lipa,   y = 6, label = "LIPA", size = 4, family = "Times New Roman",
             label.size = 0.5, fill = "white", color = "black") + 
    annotate("label", x = mpa,   y = 6, label = "MPA", size = 4, family = "Times New Roman",
             label.size = 0.5, fill = "white", color = "black") + 
    annotate("label", x = vpa,   y = 6, label = "VPA", size = 4, family = "Times New Roman",           
             label.size = 0.5, fill = "white", color = "black") + 
    
    
    labs( title = title,
          y = "Time spent in each intensity (min/day)",
          x = "Intensity distribution (counts/60s)"
    ) +
    scale_x_continuous(breaks = c(1, seq(800, 8000, 800)), , expand = c(0, 0))+
    scale_y_continuous(limits = c(0,8), expand = c(0, 0))+
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      panel.grid = element_blank(), 
      plot.title = element_text(face = "bold", size = 17,hjust = 0.5),
      strip.text = element_text(size = 13, face = "bold"),
      axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.ticks = element_line(color = "black", size = 0.8),
      axis.ticks.length = unit(0.2, "cm"),
      axis.line = element_line(color = "black")
    )
}

load("C:/Users/j_lopez/Desktop/paper2/analysis/60s/fda/data/act_60.rda") # data derived from code 14_FDA_DISTRIBUTION

data_presc <- act %>%
  filter(age<6) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=0) %>%
  filter(x<8001)
data_presc <- rbind(data.frame(x = 0, acc = 8, age = 0), data_presc)

data_child <- act %>%
  filter(age>=6 & age<11) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=1) %>%
  filter(x<8001)
data_child <- rbind(data.frame(x = 0, acc = 8, age = 1), data_child)

data_adoles <- act %>%
  filter(age>=11 & age<18) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=2) %>%
  filter(x<8001)
data_adoles <- rbind(data.frame(x = 0, acc = 8, age = 2), data_adoles)

data_younger<- act %>%
  filter(age>=18 & age<45) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=3) %>%
  filter(x<8001)
data_younger <- rbind(data.frame(x = 0, acc = 8, age = 3), data_younger)

data_middle <- act %>%
  filter(age>=45 & age<65) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=4) %>%
  filter(x<8001)
data_middle <- rbind(data.frame(x = 0, acc = 8, age = 4), data_middle)

data_older <- act %>%
  filter(age>=65) %>%
  group_by(x) %>%
  summarise(acc= mean(A_i, na.rm = TRUE)) %>%
  mutate(age=5) %>%
  filter(x<8001)
data_older <- rbind(data.frame(x = 0, acc = 8, age = 4), data_older)

plot_presc_60 <- plot_activity_distribution_60(data_presc, title = "Pre-schoolers (3-5y)", age_col = rgb(0.27, 0.95, 0.95, alpha = 0.5),
                                            cut1 = 100, cut2 = 2292, cut3 = 4007,
                                            lipa = 1200, mpa = 3200, vpa = 6400)
plot_child_60 <- plot_activity_distribution_60(data_child, title = "Children (6-10y)", age_col = rgb(1, 1, 0, alpha = 0.5),
                                            cut1 = 100, cut2 = 2292, cut3 = 4007,
                                            lipa = 1200, mpa = 3200, vpa = 6400)
plot_adoles_60 <- plot_activity_distribution_60(data_adoles, title = "Adolescents (11-18y)", age_col = rgb(0.97, 0.73, 0.08, alpha = 0.5),
                                             cut1 = 100, cut2 = 2292, cut3 = 4007,
                                             lipa = 1200, mpa = 3200, vpa = 6400)
plot_younger_60 <- plot_activity_distribution_60(data_younger, title = "Younger adults (19-44y)", age_col = rgb(0.5, 0.2, 0.02, alpha = 0.5),
                                              cut1 = 100, cut2 = 2019, cut3 = 5998,
                                              lipa = 1000, mpa = 4000, vpa = 7200)
plot_middle_60 <- plot_activity_distribution_60(data_middle, title = "Middle adults (45-64y)", age_col = rgb(0.7, 0.07, 0.96, alpha = 0.5),
                                             cut1 = 100, cut2 = 2019, cut3 = 5998,
                                             lipa = 1000, mpa = 4000, vpa = 7200)
plot_older_60 <- plot_activity_distribution_60(data_older, title = "Older adults (65-90y)", age_col = rgb(0, 1, 0, alpha = 0.5),
                                            cut1 = 100, cut2 = 2019, cut3 = 5998,
                                            lipa = 1000, mpa = 4000, vpa = 7200)

(plot_presc_60 | plot_child_60 | plot_adoles_60) / 
  (plot_younger_60 | plot_middle_60 | plot_older_60)

((plot_presc | plot_child | plot_adoles) /
    plot_spacer() /
    (plot_younger | plot_middle | plot_older)) +
  plot_layout(heights = c(1, 0.01, 1))

