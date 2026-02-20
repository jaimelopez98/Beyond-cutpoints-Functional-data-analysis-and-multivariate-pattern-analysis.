# Script name: 17_FDA_PLOTS.R

# Author: J.Lopez, Inserm

# Doing: Plotting the beta coefficients derived from the estimates of the association between the activity intensity distribution and body mass index 
# for each age group using scalar on function regression.
# (2026-02-23 data release)

# PACKAGES ----

library(ggplot2)
library(patchwork)
library(showtext)

# DATA ----
load("path/result_beta_presc.rda")
load("path/result_zero_presc.rda")

load("path/result_beta_child.rda")
load("path/result_zero_child.rda")

load("path/result_beta_adoles.rda")
load("path/result_zero_adoles.rda")

load("path/result_beta_younger.rda")
load("path/result_zero_younger.rda")

load("path/result_beta_middle.rda")
load("path/result_zero_middle.rda")

load("path/result_beta_older.rda")
load("path/result_zero_older.rda")

# PARAMETERS ----

labels_seq <- c(1,13, 25,50,100,200,300, 400, 500,750, 1000,1250, 1500, 1750, 2000, 2250)

title_presc = "a. Preschoolers (3-5y)"
col_presc= rgb(0.27, 0.95, 0.95, alpha = 0.5)

title_child = "b. Children (6-10y)"
col_child= rgb(1, 1, 0, alpha = 0.5)

title_adoles = "c. Adolescents (11-18y)"
col_adoles= rgb(0.97, 0.73, 0.08, alpha = 0.5)

title_younger = "d. Younger adults (19-44y)"
col_younger= rgb(0.5, 0.2, 0.02, alpha = 0.5)

title_middle = "e. Middle adults (45-64y)"
col_middle= rgb(0.7, 0.07, 0.96, alpha = 0.5)

title_older = "f. Older adults (65-90y)"
col_older= rgb(0, 1, 0, alpha = 0.5)

plot_presc <- ggplot(result_beta_presc, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_presc, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_presc, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_presc, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 573, 1002)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_presc$s[labels_seq])),
                     labels = c(0, result_beta_presc$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 15,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 28,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 39,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_presc) +
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

# CHILDREN (n=1248) ----

plot_child <- ggplot(result_beta_child, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_child, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_child, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_child, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 573, 1002)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_child$s[labels_seq])),
                     labels = c(0, result_beta_child$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 15,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 28,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 39,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_child) +
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

# ADOLESCENTS (n=466) ----

plot_adoles <- ggplot(result_beta_adoles, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_adoles, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_adoles, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_adoles, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 573, 1002)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_adoles$s[labels_seq])),
                     labels = c(0, result_beta_adoles$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 15,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 28,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 39,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",           
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_adoles) +
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

# YOUNGER ADULTS (n=1447) ----

plot_younger <- ggplot(result_beta_younger, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_younger, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_younger, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_younger, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 505, 1500)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_younger$s[labels_seq])),
                     labels = c(0, result_beta_younger$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman", 
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 13,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 31,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 42,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_younger) +
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

# MIDDLE ADULTS (n=1757) ----

plot_middle <- ggplot(result_beta_middle, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_middle, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_middle, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_middle, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 505, 1500)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_middle$s[labels_seq])),
                     labels = c(0, result_beta_middle$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman", 
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 13,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 31,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 42,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_middle) +
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

# OLDER ADULTS (n=1051) ----

plot_older <- ggplot(result_beta_older, aes(x = sqrt(s), y = value)) +
  geom_point(data = result_zero_older, aes(x = 0, y = value), shape = 19, size = 2) +
  geom_errorbar(data = result_zero_older, aes(x = 0, ymin = lower, ymax = upper),
                width = 0.5, color = "black") +
  geom_line(aes(x = sqrt(s), y = value),color = "black") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = col_older, alpha = 0.4) +
  geom_vline(xintercept = sqrt(c(25, 505, 1500)),
             linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "solid") +
  scale_x_continuous(breaks = c(0, sqrt(result_beta_older$s[labels_seq])),
                     labels = c(0, result_beta_older$s[labels_seq])) +
  scale_y_continuous(
    breaks = seq(-0.9, 0.9, by = 0.2),
    labels = seq(-0.9, 0.9, by = 0.2)) +
  coord_cartesian(ylim =c(-0.9, 0.5), xlim = c(0,43))+
  
  annotate("label", x = 1.5,  y = 0.5, label = "SB",  size = 5, family = "Times New Roman", 
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 13,   y = 0.5, label = "LIPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 31,   y = 0.5, label = "MPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  annotate("label", x = 42,   y = 0.5, label = "VPA", size = 5, family = "Times New Roman",
           label.size = 0.5, fill = "white", color = "black") + 
  labs(
    x = "Intensity distribution (count/15s)", 
    y = expression(beta ~"(" * "Intensity distribution"~")"),
    title = title_older) +
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

# PLOTS ----
plot_presc + plot_child + plot_adoles +
  plot_layout(ncol = 1)

plot_younger + plot_middle + plot_older +
  plot_layout(ncol = 1)

plot_presc + plot_younger + plot_child + 
  plot_middle + plot_adoles + plot_older +
  plot_layout(ncol = 2, nrow = 3)

plot_presc
plot_child
plot_adoles
plot_younger
plot_middle
plot_older

