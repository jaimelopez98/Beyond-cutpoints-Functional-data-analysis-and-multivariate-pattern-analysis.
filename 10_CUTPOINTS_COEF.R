# Script name: 10_CUTPOINTS_COEF.R

# Author: J.Lopez, Inserm

# Doing: Merge all the coefficients derived from the linear regression models from each set of cut-point.
# (2026-02-23 data release)

cut_15_1 <- read.xlsx("path/cutpoint1_adjusted_15.xlsx")
cut_15_2 <- read.xlsx("path/cutpoint2_adjusted_15.xlsx")
cut_15_3 <- read.xlsx("path/cutpoint3_adjusted_15.xlsx")

cutpoint_results <- cbind(cut_15_1,cut_15_2,cut_15_3) %>%
  select(SB1,SB2,SB3,
         LIPA1,LIPA2,LIPA3,
         MPA1,MPA2,MPA3,
         VPA1,VPA2,VPA3,
         MVPA1,MVPA2,MVPA3)
cutpoint_results2 <- as.data.frame(t(cutpoint_results)) %>%
  rename("Pre-schoolers (3-5y)" = "1",
         "Children (6-10y)" = "2",
         "Adolescents (11-17y)" = "3",
         "Younger adults (19-44y)" = "4",
         "Middle adults (45-64y)" = "5",
         "Older adults (+64y)" = "6")

write.xlsx(cutpoint_results2, "path/cutpoint_linear_results_adjusted_15.xlsx", 
           colNames = TRUE, rowNames = TRUE, append = FALSE)
