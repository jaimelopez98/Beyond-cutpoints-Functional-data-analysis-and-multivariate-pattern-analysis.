# Script name: 06_PROPORTION.R

# Author: J.López, Inserm

# Doing: 
#   *Plotting the proportion of time spent in each physical activity intensity for each age group depending on the cut-point used.

# PACKAGES

library(ggplot2)
library(dplyr)
library(openxlsx)
library(patchwork)
library(showtext)


data <- read.xlsx("path/data.csv")

data_youth <- data %>% filter (AGE<3) %>%
  mutate(
    INTENSITY = factor(INTENSITY,
                       levels = c(2,1,0),
                       labels = c("MVPA", "LIPA","SB" )),
    CUTPOINT = factor(CUTPOINT,
                      levels = c(0,1,2),
                      labels = c("Evenson", "Romanzini", "Puyau")),
    AGE = factor(AGE,
                 levels = c(0,1,2),
                 labels = c("a. Preschoolers (3-5y)", "b. Children (6-10y)", "c. Adolescents (11-18y)")))%>%
  mutate(PROPORTION= round(PROPORTION,1))

data_adulthood <- data %>% filter (AGE>2) %>%
  mutate(
    INTENSITY = factor(INTENSITY,
                       levels = c(2,1,0),
                       labels = c("MVPA", "LIPA","SB" )),
    CUTPOINT = factor(CUTPOINT,
                      levels = c(3,4,5),
                      labels = c("Troiano", "Matthews", "Swartz")),
    AGE = factor(AGE,
                 levels = c(3,4,5),
                 labels = c("d. Younger Adults (19-44y)", "e. Middle Adults (45-64y)", "f. Older Adults (65-90y)")))%>%
  mutate(PROPORTION= round(PROPORTION,1))

plot_youth <- ggplot(data_youth, aes(x = CUTPOINT, y = PROPORTION, fill = INTENSITY)) + 
  geom_bar(stat = "identity", position = "stack",color = "black") + 
  geom_text(aes(label = PROPORTION), 
            position = position_stack(vjust = 0.5), 
            size = 4,                             
            color = "black",
            family = "Times New Roman") +
  facet_wrap(~AGE) +
  scale_fill_manual(
    values = c(
      "SB" = "#9ecae1",  
      "LIPA"  = "#3182bd",  
      "MVPA"  = "#08519c"   
    )
  ) +
  labs(x = "Set of cut-points used", y = "Proportion (%) of time spent in each intensity (min/day)", fill = "Intensity level") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0))+
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    legend.title = element_text(size = 14, family = "Times New Roman"),
    legend.text  = element_text(size = 12, family = "Times New Roman"),
    panel.grid = element_blank(), 
    strip.text = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(color = "black")
  )
  
plot_adulthood <- ggplot(data_adulthood, aes(x = CUTPOINT, y = PROPORTION, fill = INTENSITY)) + 
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = PROPORTION), 
            position = position_stack(vjust = 0.5),  
            size = 4,                               
            color = "black",
            family = "Times New Roman") +
  facet_wrap(~AGE) +
  scale_fill_manual(values = c("SB" = "#9ecae1", "LIPA"  = "#3182bd", "MVPA"  = "#08519c")) +
  labs(x = "Set of cut-points used", y = "Proportion (%) of time spent in each intensity (min/day)", fill = "Intensity level") +
  scale_y_continuous(limits = c(0, 101), expand = c(0, 0))+
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    panel.grid = element_blank(), 
    legend.title = element_text(size = 16, family = "Times New Roman"),
    legend.text  = element_text(size = 14, family = "Times New Roman"),
    strip.text = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.ticks = element_line(color = "black", size = 0.8),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line = element_line(color = "black")
  )

plot_youth 

plot_adulthood 


