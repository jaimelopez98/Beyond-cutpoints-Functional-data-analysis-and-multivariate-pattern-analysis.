# Script name: 08_CUTPOINT_ANOVA.R

# Author: J.López, Inserm

# Doing: 
#   * Evaluate the differences in time at each intensity level across the three cut-point sets using pairwise ANOVA.

# PACKAGES

library(tidyr)
library(dplyr)
library(ggplot2)

data <- read.csv("path/data_ref.csv")

presc_data <- data %>% filter(age<6) %>% select(stno, ends_with("min_day"))
child_data <- data %>% filter(age>=6 & age<11) %>% select(stno, ends_with("min_day"))
adoles_data <- data %>% filter(age>=11 & age<19) %>% select(stno, ends_with("min_day"))
younger_data <- data %>% filter(age>=19 & age<45) %>% select(stno, ends_with("min_day"))
middle_data <- data %>% filter(age>=45 & age<65) %>% select(stno, ends_with("min_day"))
older_data <- data %>% filter(age>=65) %>% select(stno, ends_with("min_day"))

# PRE-SCHOOLERS ----

presc_SB <- presc_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
presc_LIPA <- presc_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
presc_MPA <- presc_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
presc_VPA <- presc_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
presc_MVPA <- presc_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_presc_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = presc_SB)
aov_presc_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = presc_LIPA)
aov_presc_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = presc_MPA)
aov_presc_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = presc_VPA)
aov_presc_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = presc_MVPA)

summary(aov_presc_SB)
summary(aov_presc_LIPA)
summary(aov_presc_MPA)
summary(aov_presc_VPA)
summary(aov_presc_MVPA)

pairwise.t.test(
  x = presc_SB$SB_time,
  g = presc_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = presc_LIPA$LIPA_time,
  g = presc_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = presc_MPA$MPA_time,
  g = presc_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = presc_VPA$VPA_time,
  g = presc_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = presc_MVPA$MVPA_time,
  g = presc_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
# CHILDREN ----

child_SB <- child_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
child_LIPA <- child_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
child_MPA <- child_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
child_VPA <- child_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
child_MVPA <- child_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_child_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = child_SB)
aov_child_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = child_LIPA)
aov_child_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = child_MPA)
aov_child_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = child_VPA)
aov_child_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = child_MVPA)

summary(aov_child_SB)
summary(aov_child_LIPA)
summary(aov_child_MPA)
summary(aov_child_VPA)
summary(aov_child_MVPA)

pairwise.t.test(
  x = child_SB$SB_time,
  g = child_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = child_LIPA$LIPA_time,
  g = child_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = child_MPA$MPA_time,
  g = child_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = child_VPA$VPA_time,
  g = child_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = child_MVPA$MVPA_time,
  g = child_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

# ADOLESCENTS ----

adoles_SB <- adoles_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
adoles_LIPA <- adoles_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
adoles_MPA <- adoles_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
adoles_VPA <- adoles_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
adoles_MVPA <- adoles_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_adoles_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = adoles_SB)
aov_adoles_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = adoles_LIPA)
aov_adoles_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = adoles_MPA)
aov_adoles_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = adoles_VPA)
aov_adoles_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = adoles_MVPA)

summary(aov_adoles_SB)
summary(aov_adoles_LIPA)
summary(aov_adoles_MPA)
summary(aov_adoles_VPA)
summary(aov_adoles_MVPA)

pairwise.t.test(
  x = adoles_SB$SB_time,
  g = adoles_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = adoles_LIPA$LIPA_time,
  g = adoles_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = adoles_MPA$MPA_time,
  g = adoles_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = adoles_VPA$VPA_time,
  g = adoles_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = adoles_MVPA$MVPA_time,
  g = adoles_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

# YOUNGER ADULTS ----

younger_SB <- younger_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
younger_LIPA <- younger_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
younger_MPA <- younger_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
younger_VPA <- younger_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
younger_MVPA <- younger_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_younger_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = younger_SB)
aov_younger_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = younger_LIPA)
aov_younger_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = younger_MPA)
aov_younger_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = younger_VPA)
aov_younger_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = younger_MVPA)

summary(aov_younger_SB)
summary(aov_younger_LIPA)
summary(aov_younger_MPA)
summary(aov_younger_VPA)
summary(aov_younger_MVPA)

pairwise.t.test(
  x = younger_SB$SB_time,
  g = younger_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = younger_LIPA$LIPA_time,
  g = younger_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = younger_MPA$MPA_time,
  g = younger_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = younger_VPA$VPA_time,
  g = younger_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = younger_MVPA$MVPA_time,
  g = younger_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
# MIDDLE ADULTS ----

middle_SB <- middle_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
middle_LIPA <- middle_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
middle_MPA <- middle_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
middle_VPA <- middle_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
middle_MVPA <- middle_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_middle_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = middle_SB)
aov_middle_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = middle_LIPA)
aov_middle_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = middle_MPA)
aov_middle_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = middle_VPA)
aov_middle_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = middle_MVPA)

summary(aov_middle_SB)
summary(aov_middle_LIPA)
summary(aov_middle_MPA)
summary(aov_middle_VPA)
summary(aov_middle_MVPA)

pairwise.t.test(
  x = middle_SB$SB_time,
  g = middle_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = middle_LIPA$LIPA_time,
  g = middle_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = middle_MPA$MPA_time,
  g = middle_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = middle_VPA$VPA_time,
  g = middle_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = middle_MVPA$MVPA_time,
  g = middle_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

# OLDER ADULTS ----

older_SB <- older_data %>%
  pivot_longer(
    cols = c(SB1_min_day, SB2_min_day, SB3_min_day),
    names_to = "cutpoint",
    values_to = "SB_time"
  )
older_LIPA <- older_data %>%
  pivot_longer(
    cols = c(LIPA1_min_day, LIPA2_min_day, LIPA3_min_day),
    names_to = "cutpoint",
    values_to = "LIPA_time"
  )
older_MPA <- older_data %>%
  pivot_longer(
    cols = c(MPA1_min_day, MPA2_min_day, MPA3_min_day),
    names_to = "cutpoint",
    values_to = "MPA_time"
  )
older_VPA <- older_data %>%
  pivot_longer(
    cols = c(VPA1_min_day, VPA2_min_day, VPA3_min_day),
    names_to = "cutpoint",
    values_to = "VPA_time"
  )
older_MVPA <- older_data %>%
  pivot_longer(
    cols = c(MVPA1_min_day, MVPA2_min_day, MVPA3_min_day),
    names_to = "cutpoint",
    values_to = "MVPA_time"
  )

aov_older_SB <- aov(SB_time ~ cutpoint + Error(stno/cutpoint), data = older_SB)
aov_older_LIPA <- aov(LIPA_time ~ cutpoint + Error(stno/cutpoint), data = older_LIPA)
aov_older_MPA <- aov(MPA_time ~ cutpoint + Error(stno/cutpoint), data = older_MPA)
aov_older_VPA <- aov(VPA_time ~ cutpoint + Error(stno/cutpoint), data = older_VPA)
aov_older_MVPA <- aov(MVPA_time ~ cutpoint + Error(stno/cutpoint), data = older_MVPA)

summary(aov_older_SB)
summary(aov_older_LIPA)
summary(aov_older_MPA)
summary(aov_older_VPA)
summary(aov_older_MVPA)

pairwise.t.test(
  x = older_SB$SB_time,
  g = older_SB$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

pairwise.t.test(
  x = older_LIPA$LIPA_time,
  g = older_LIPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = older_MPA$MPA_time,
  g = older_MPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = older_VPA$VPA_time,
  g = older_VPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)
pairwise.t.test(
  x = older_MVPA$MVPA_time,
  g = older_MVPA$cutpoint,
  paired = TRUE,
  p.adjust.method = "bonferroni"
)

# Pairwise
t.test(
  older_data$VPA1_min_day,
  older_data$VPA2_min_day,
  paired = TRUE
)

