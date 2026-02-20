# Beyond-cutpoints-Functional-data-analysis-and-multivariate-pattern-analysis.

This repository includes the codes needed to replicate the analysis done in the paper: Beyond Cut-points: Using Advanced Statistical Approaches to refine the understanding of the Associations between Accelerometer-Measured Activity Intensity and Health. A LABDA project. (doi:xxx)

This study aims to examine the added value of investigating the high-resolution activity intensity distribution using advanced statistical approaches (Functional data analysis and Multivariate pattern analysis), rather than cut-point-derived intensities, in its associations with body mass index (BMI) across age (3-90y). 

It includes 3 different approaches:

  1) Multiple Linear regression using 3 different sets of cut-points for youths (3-17y) and adulthood (18-90y).
  2) Scalar-on-function regression, assessing associations across the continuous activity intensity distribution (0–2000 counts/15s).
  3) Multivariate pattern analysis, investigating the associations across a large number (n=19) of PA intensity variables.

Data were analysed using R 3.6.1 (http://www.r-project.org), analyses required downloading of the following packages:
- *GGIR* for accelerometer data processing (version 3.0.9, https://cran.r-project.org/web/packages/GGIR/vignettes/GGIR.html)
- *ks* for kernel smoothing (version 1.14.2, https://cran.r-project.org/web/packages/ks/ks.pdf)
- *mgcv* for all scalar-on-function regressions (version 0.1.36, https://cran.r-project.org/web/packages/refund/refund.pdf)
- *pracma* for trapezoidal integration of functional coefficients (version 2.4.4, https://cran.r-project.org/web/packages/pracma/pracma.pdf)
- *mvpaShiny* for Multivariate pattern analysis (version 0.1.2, https://www.mvpashiny.org) 
- *tidyfun* for data preparation (version 0.0.98, https://tidyfun.github.io/tidyfun/index.html)
- *ggplot2* for illustrating plots of functional coefficients (version 3.5.1, https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf)

![Flowchart of the procedures for the paper](<img width="4345" height="2289" alt="Image" src="https://github.com/user-attachments/assets/0b39c8de-3492-42c8-a573-fee0ecb8df09" />)

More details on each step are provided in the following sections:

## Step 0 - Data preparation

- *01_DATA_PROCESSING*. Data processing, a modified code to process accelerometer data using GGIR packages, including the combination of two algorithms to detect the sleep window in hip-count accelerometer devices.
- *02_DATA_EXTRACTION*. Data extraction, this code was used to identify the waking window, identify valid participants, and calculate the time spent in SB and each PA intensity using 6 sets of cut-points (3 for youths and 3 for adulthood) and extract the time spent in each PA variable needed for the multivariate pattern analysis. This code is run for each study separately and then merged into one document.

## Step 1 - Exploratory analysis 

- *03_COVARIATES*. Covariates preparation.
- *04_DEMOGRAPHIC*. Demographic characteristics of the study population. (Table 1)
- *05_DISTRIBUTION*. Distribution of activity intensity (counts/15s) in youth (A) and adulthood (B). (Figure 1)
- *06_PROPORTION*. Proportion of time spent in each activity intensity level (counts/15s) using different cut-points in youth (a-c) and adulthood (d-f). (Figure 2)
- *07_ACCELEROMETER_DATA*. Mean and standard deviation of accelerometer data (counts/15s) in youth and adulthood using different cut-points (Table S1 and S2)
- *08_INTERACTIONS*. Interactions between age and time in activity intensities (counts/15s) for the associations with body mass index (kg/m²) in youth and adulthood. (Table S3 and S4)

## Step 2 - Cut-point approach (Multiple Linear regression) 

- *09_CUTPOINTS_DATA*. Preparation of data for each age group.
- *10_CUTPOINTS_MODELS*. Modelling for SB and each PA intensity for each age group.
- *11_CUTPOINTS_DATA*. Extracting coefficients from linear regression models (Table S4 and S5).

## Step 3 - Functional data analysis (Scalar-on-function regression)

- 12_ACT_KERNEL. Characterising the probability density function of each individual using the kernel smoothing method.
- *13_ACT_STANDARD*. Standardising kernel densities.
- *14_ACT_DISTRIBUTION*. Estimating activity distribution.
- *15_ACT_DATA*. Preparation of data for modelling in each age group.
- *16_ACT_MODELS*. Modelling for each age group.
- *17_ACT_PLOTS*. Plot the activity intensity distribution for each age group using functional data analysis. (Figures 3 and 5)

## Step 4 -  Multivariate pattern analysis

- For multivariate pattern analysis, we use the Shiny app (https://liningtonlab.github.io/mvpaShiny_documentation/publication/R_script/), and we extract the multivariate correlation coefficients for each age group.
- *18_MvPA_PLOTS*. Plot the activity intensity distribution for each age group using the multivariate pattern analysis. (Figures 4 and 6)

## Step 5 - Sensitivity analysis (60s epoch)
- For the sensitivity, the same codes are used to replicate all the previous analyses using a 60s instead of 15s epoch.
