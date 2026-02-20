# Script name: 18_MVPA_PLOTS.R

# Author: J.Lopez, Inserm

# Doing: Plotting the beta coefficients derived from the estimates of the association between the activity intensity distribution and body mass index 
# for each age group using multivariate pattern analysis.
# (2026-02-23 data release)

library(ggplot2)
library(dplyr)
library(patchwork)

mpa_15_plot <- function(data, mpa_cuts, mpa_labels,col_plot, title) {
  ggplot(data, aes(x = acc, y = Median)) +
    geom_bar(stat = "identity", width = 0.8, show.legend = FALSE,
             fill = col_plot) + 
    geom_errorbar(aes(ymin = CL_lower, ymax = CL_upper), 
                  position = position_dodge(width = 0.9), 
                  width = 0.5) + 
    geom_hline(yintercept = 0, size = 0.5) +
    geom_vline(xintercept = 5.5, linetype = "dashed", color = "red", size = 0.5) + # for adults 
    geom_vline(xintercept = 15.5, linetype = "dashed", color = "red", size = 0.5) + # for adults
    geom_vline(xintercept = 19.5, linetype = "dashed", color = "red", size = 0.5) + # for adults 23.5
    
    annotate("text", x = 2,  y = 0.25, label = "SB",  size = 5, family = "Times New Roman") + # for adults
    annotate("text", x = 10.5,   y = 0.25, label = "LIPA", size = 5, family = "Times New Roman") + # for adults
    annotate("text", x = 17.5,   y = 0.25, label = "MPA", size = 5, family = "Times New Roman") + # for adults
    annotate("text", x = 22.5,   y = 0.25, label = "VPA", size = 5, family = "Times New Roman") + # for adults 24.5
    
    labs(
      x = "Intensity distribution (counts/15s)", 
      y = "Multivariate correlation coefficient", 
      title = title
    ) + 
    scale_x_continuous(
      breaks = mpa_cuts,
      labels = mpa_labels
    ) +
    scale_y_continuous(
      limits = c(-0.35, 0.35),
      breaks = seq(-0.35, 0.35, by = 0.1)
    ) +
    theme_minimal() +  
    theme(
      text = element_text(family = "serif"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "black", linewidth = 0.2), 
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white", color = NA),
      axis.line = element_blank(),
      axis.title.x = element_text(size = 15),  
      axis.title.y = element_text(size = 15), 
      plot.title = element_text(face = "bold", size = 17,hjust = 0.5),
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
      axis.text.y = element_text(size = 12),  
      strip.text = element_text(face = "bold", size = 15),
      axis.ticks = element_line(color = "black"),
      strip.background = element_rect(fill = "white")
    )
}

mpa_labels <- c("0", "1-3", "4-6", "7-12", "13-18", "19-24", "25-31",
                "32-37","38-49","50-74","75-99","100-124","125-199","200-249","250-374",
                "375-499","500-624","625-749","750-874", "875-999", 
                "1000-1124", "1125-1249", "1250-1374","1375-1499", "1500-1749", "1750-1999")

mpa_cuts <- 0:25

presc_data <- read.csv("path/presc_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)


child_data <- read.csv("path/child_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)

adoles_data <- read.csv("path/adoles_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)

younger_data <- read.csv("path/younger_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)

middle_data <- read.csv("path/middle_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)

older_data <- read.csv("path/older_adjusted_Multivariate correlation coefficient_MC-PLSR_2025-10-02.csv") %>% 
  mutate(
    acc = mpa_cuts)


plot_presc <- mpa_15_plot(presc_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(0.27, 0.95, 0.95, alpha = 0.5), title ="a. Preschoolers (3-5y)")

plot_child <- mpa_15_plot(child_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(1, 1, 0, alpha = 0.5), title ="b. Children (6-10y)")

plot_adoles <- mpa_15_plot(adoles_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(0.97, 0.73, 0.08, alpha = 0.5), title ="c. Adolescents (11-18y)")

plot_younger <- mpa_15_plot(younger_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(0.5, 0.2, 0.02, alpha = 0.5), title ="d. Younger adults (19-44y)")

plot_middle <- mpa_15_plot(middle_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(0.7, 0.07, 0.96, alpha = 0.5), title ="e. Middle Adults (45-64y)")

plot_older <- mpa_15_plot(older_data,mpa_cuts = mpa_cuts, mpa_labels = mpa_labels,col_plot = rgb(0, 1, 0, alpha = 0.5), title ="e. Older adults (65-90y)")


plot_presc + plot_child + plot_adoles +
  plot_layout(ncol = 1)

plot_younger + plot_middle + plot_older +
  plot_layout(ncol = 1)

