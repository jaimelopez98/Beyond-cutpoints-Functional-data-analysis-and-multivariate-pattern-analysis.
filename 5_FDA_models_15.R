# PACKAGES ----

library(mgcv)
library(dplyr)

# DATA ----
load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_presc.rda")
data_presc$Y_Activity <- data_presc$Y_Activity
data_presc$ACC_0_0 <- data_presc$ACC_0_0

load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_child.rda")
data_child$Y_Activity <- data_child$Y_Activity
data_child$ACC_0_0 <- data_child$ACC_0_0

data_child <- data_child %>% filter(stno != 465 & stno != 666 & stno != 669)

load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_adoles.rda")
data_adoles$Y_Activity <- data_adoles$Y_Activity
data_adoles$ACC_0_0 <- data_adoles$ACC_0_0

data_adoles <- data_adoles %>% filter (stno != 1643)

load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_younger.rda")
data_younger$Y_Activity <- data_younger$Y_Activity
data_younger$ACC_0_0 <- data_younger$ACC_0_0

load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_middle.rda")
data_middle$Y_Activity <- data_middle$Y_Activity
data_middle$ACC_0_0 <- data_middle$ACC_0_0

load("C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/models/data_older.rda")
data_older$Y_Activity <- data_older$Y_Activity
data_older$ACC_0_0 <- data_older$ACC_0_0

# PARAMETERS ----
pts <- round(seq(1:2340))
c_value <- 2340
nboot <- 1000

# PRESCHOOLERS (n=1137) ----

X <- data_presc$Y_Activity
N <- nrow(data_presc)
  
# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)
  
# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))
  
  # Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
                c(1, rep(c(4, 2), len = c_value - 2), 1),
              c_value, 1)
  
  # Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))
  
  # Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L
  
  # Prepare data frame for gam
df_mgcv <- data.frame(
    X_L = I(X_L),
    S = I(S),
    y = data_presc[["bmi"]],
    gender = data_presc$gender,
    age = data_presc$age,
    edu_cat1 = data_presc$edu_cat1,
    edu_cat2 = data_presc$edu_cat2,
    season_cat1 = data_presc$season_cat1,
    season_cat2 = data_presc$season_cat2,
    season_cat3 = data_presc$season_cat3,
    study_cat1 = data_presc$study_cat1,
    study_cat2 = data_presc$study_cat2,
    study_cat3 = data_presc$study_cat3,
    zero = data_presc$ACC_0_0)
  
  # Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 + study_cat1, 
                 method = "REML", data = df_mgcv)

  # Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  
 
 # Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_presc$ACC_0_0)
wear_pred <- mean(data_presc$wear_min_day) 

  # Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 4, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0, study_cat1=0)
  
  # Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)
  
  # Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 10]
se_beta_hat <- coef_est$se.fit[, 10]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

  # Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]
  
beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]
  
# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
  for (i in 1:nboot) {
    beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
    beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
  }

beta_mat_boot_presc <- beta_mat_boot
save(beta_mat_boot_presc, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_presc.rda")

  # Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)
  
beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_presc <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_presc <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_presc, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_presc.rda")
#save(zero_presc, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_presc.rda")

# CHILDREN (n=1245) ----

X <- data_child$Y_Activity
N <- nrow(data_child)

# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)

# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))

# Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
              c(1, rep(c(4, 2), len = c_value - 2), 1),
            c_value, 1)

# Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))

# Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L

# Prepare data frame for gam
df_mgcv <- data.frame(
  X_L = I(X_L),
  S = I(S),
  y = data_child[["bmi"]],
  gender = data_child$gender,
  age = data_child$age,
  edu_cat1 = data_child$edu_cat1,
  edu_cat2 = data_child$edu_cat2,
  season_cat1 = data_child$season_cat1,
  season_cat2 = data_child$season_cat2,
  season_cat3 = data_child$season_cat3,
  study_cat1 = data_child$study_cat1,
  study_cat2 = data_child$study_cat2,
  study_cat3 = data_child$study_cat3,
  zero = data_child$ACC_0_0)

# Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3 + study_cat1 + study_cat2, 
               method = "REML", data = df_mgcv)

# Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  

# Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_child$ACC_0_0)
wear_pred <- mean(data_child$wear_min_day) 

# Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 8, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0, study_cat1 = 0, study_cat2 = 0)

# Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)

# Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 11]
se_beta_hat <- coef_est$se.fit[, 11]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

# Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]

beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]

# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
for (i in 1:nboot) {
  beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
  beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
}

beta_mat_boot_child <- beta_mat_boot
save(beta_mat_boot_child, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_child.rda")

# Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)

beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_child <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_child <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_child, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_child.rda")
#save(zero_child, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_child.rda")

# ADOLESCENTS (n=465) ----

X <- data_adoles$Y_Activity
N <- nrow(data_adoles)

# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)

# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))

# Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
              c(1, rep(c(4, 2), len = c_value - 2), 1),
            c_value, 1)

# Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))

# Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L

# Prepare data frame for gam
df_mgcv <- data.frame(
  X_L = I(X_L),
  S = I(S),
  y = data_adoles[["bmi"]],
  gender = data_adoles$gender,
  age = data_adoles$age,
  edu_cat1 = data_adoles$edu_cat1,
  edu_cat2 = data_adoles$edu_cat2,
  season_cat1 = data_adoles$season_cat1,
  season_cat2 = data_adoles$season_cat2,
  season_cat3 = data_adoles$season_cat3,
  study_cat1 = data_adoles$study_cat1,
  study_cat2 = data_adoles$study_cat2,
  study_cat3 = data_adoles$study_cat3,
  zero = data_adoles$ACC_0_0)

# Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3, 
               method = "REML", data = df_mgcv)

# Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  

# Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_adoles$ACC_0_0)
wear_pred <- mean(data_adoles$wear_min_day) 

# Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 14, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0)

# Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)

# Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 9]
se_beta_hat <- coef_est$se.fit[, 9]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

# Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]

beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]

# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
for (i in 1:nboot) {
  beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
  beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
}

beta_mat_boot_adoles <- beta_mat_boot
save(beta_mat_boot_adoles, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_adoles.rda")

# Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)

beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_adoles <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_adoles <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_adoles, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_adoles.rda")
#save(zero_adoles, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_adoles.rda")

# YOUNGER ADULTS (n=1447) ----

X <- data_younger$Y_Activity
N <- nrow(data_younger)

# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)

# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))

# Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
              c(1, rep(c(4, 2), len = c_value - 2), 1),
            c_value, 1)

# Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))

# Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L

# Prepare data frame for gam
df_mgcv <- data.frame(
  X_L = I(X_L),
  S = I(S),
  y = data_younger[["bmi"]],
  gender = data_younger$gender,
  age = data_younger$age,
  edu_cat1 = data_younger$edu_cat1,
  edu_cat2 = data_younger$edu_cat2,
  season_cat1 = data_younger$season_cat1,
  season_cat2 = data_younger$season_cat2,
  season_cat3 = data_younger$season_cat3,
  study_cat1 = data_younger$study_cat1,
  study_cat2 = data_younger$study_cat2,
  study_cat3 = data_younger$study_cat3,
  zero = data_younger$ACC_0_0)

# Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3, 
               method = "REML", data = df_mgcv)

# Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  

# Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_younger$ACC_0_0)
wear_pred <- mean(data_younger$wear_min_day) 

# Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 30, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0)

# Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)

# Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 9]
se_beta_hat <- coef_est$se.fit[, 9]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

# Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]

beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]

# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
for (i in 1:nboot) {
  beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
  beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
}

beta_mat_boot_younger <- beta_mat_boot
save(beta_mat_boot_younger, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_younger.rda")

# Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)

beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_younger <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_younger <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_younger, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_younger.rda")
#save(zero_younger, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_younger.rda")

# MIDDLE ADULTS (n=1757) ----

X <- data_middle$Y_Activity
N <- nrow(data_middle)

# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)

# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))

# Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
              c(1, rep(c(4, 2), len = c_value - 2), 1),
            c_value, 1)

# Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))

# Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L

# Prepare data frame for gam
df_mgcv <- data.frame(
  X_L = I(X_L),
  S = I(S),
  y = data_middle[["bmi"]],
  gender = data_middle$gender,
  age = data_middle$age,
  edu_cat1 = data_middle$edu_cat1,
  edu_cat2 = data_middle$edu_cat2,
  season_cat1 = data_middle$season_cat1,
  season_cat2 = data_middle$season_cat2,
  season_cat3 = data_middle$season_cat3,
  study_cat1 = data_middle$study_cat1,
  study_cat2 = data_middle$study_cat2,
  study_cat3 = data_middle$study_cat3,
  zero = data_middle$ACC_0_0)

# Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3, 
               method = "REML", data = df_mgcv)

# Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  

# Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_middle$ACC_0_0)
wear_pred <- mean(data_middle$wear_min_day) 

# Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 30, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0)

# Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)

# Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 9]
se_beta_hat <- coef_est$se.fit[, 9]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

# Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]

beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]

# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
for (i in 1:nboot) {
  beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
  beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
}

beta_mat_boot_middle <- beta_mat_boot
save(beta_mat_boot_middle, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_middle.rda")

# Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)

beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_middle <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_middle <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_middle, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_middle.rda")
#save(zero_middle, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_middle.rda")

# OLDER ADULTS (n=1051) ----

X <- data_older$Y_Activity
N <- nrow(data_older)

# Vector containing functional domain of observed data
s_vec <- seq(1, c_value)

# Matrix containing domain for each person (row)
S <- kronecker(matrix(1, N, 1), t(s_vec))

# Vector quadrature weights (Simpson’s rule)
q <- matrix((s_vec[length(s_vec)] - s_vec[1]) / length(s_vec) / 3 *
              c(1, rep(c(4, 2), len = c_value - 2), 1),
            c_value, 1)

# Matrix containing quadrature weights for each person (row)
L <- kronecker(matrix(1, N, 1), t(q))

# Functional predictor multiplied by quadrature weights, elementwise
X_L <- X * L

# Prepare data frame for gam
df_mgcv <- data.frame(
  X_L = I(X_L),
  S = I(S),
  y = data_older[["bmi"]],
  gender = data_older$gender,
  age = data_older$age,
  edu_cat1 = data_older$edu_cat1,
  edu_cat2 = data_older$edu_cat2,
  season_cat1 = data_older$season_cat1,
  season_cat2 = data_older$season_cat2,
  season_cat3 = data_older$season_cat3,
  study_cat1 = data_older$study_cat1,
  study_cat2 = data_older$study_cat2,
  study_cat3 = data_older$study_cat3,
  zero = data_older$ACC_0_0)

# Fit the GAM
gam_fit <- gam(y ~ s(S, by = X_L, bs = "tp") + zero + gender + age + edu_cat1 + edu_cat2 + season_cat1 + season_cat2 + season_cat3, 
               method = "REML", data = df_mgcv)

# Extract zero count coefficients and confidence intervals
zero_hat <- summary(gam_fit)$p.table [2,1]
se_zero_hat <- summary(gam_fit)$p.table [2,2]

zero_hat_LB <- zero_hat - qnorm(0.975) * se_zero_hat
zero_hat_UB <- zero_hat + qnorm(0.975) * se_zero_hat  

# Set up grid for prediction
s_pred <- seq(1, c_value, len = c_value)
zero_pred <- mean(data_older$ACC_0_0)
wear_pred <- mean(data_older$wear_min_day) 

# Required data inputs in a data frame
df_pred <- data.frame(S = s_pred, X_L = 1, zero = zero_pred, gender = 1, 
                      age = 78, edu_cat1 = 1, edu_cat2 = 0, season_cat1 = 1, season_cat2 = 0, 
                      season_cat3 = 0)

# Predict terms
coef_est <- predict(gam_fit, newdata = df_pred, type = "terms", se.fit = TRUE)

# Extract point estimates and standard errors

beta_hat <- coef_est$fit[, 9]
se_beta_hat <- coef_est$se.fit[, 9]

beta_hat_LB_unadjusted <- beta_hat - qnorm(0.975) * se_beta_hat
beta_hat_UB_unadjusted <- beta_hat + qnorm(0.975) * se_beta_hat

# Design matrix
lpmat <- predict(gam_fit, newdata = df_pred, type = "lpmatrix")
inx_beta <- which(grepl("s\\(S\\):X_L\\.[0-9]+", dimnames(lpmat)[[2]]))
Bmat <- lpmat[, inx_beta]

beta_sp <- coef(gam_fit)[inx_beta]
Vbeta_sp <- vcov(gam_fit)[inx_beta, inx_beta]

# Bootstrap
beta_mat_boot <- matrix(NA, nboot, length(s_pred))
for (i in 1:nboot) {
  beta_sp_i <- MASS::mvrnorm(n = 1, mu = beta_sp, Sigma = Vbeta_sp)
  beta_mat_boot[i, ] <- Bmat %*% beta_sp_i
}

beta_mat_boot_older <- beta_mat_boot
save(beta_mat_boot_older, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_matrix_older.rda")

# Max statistic and global confidence band
dvec <- apply(beta_mat_boot, 1, function(x) max(abs(x - beta_hat) / se_beta_hat))
Z_global <- quantile(dvec, 0.95)

beta_hat_LB_global <- beta_hat - Z_global * se_beta_hat
beta_hat_UB_global <- beta_hat + Z_global * se_beta_hat

beta_older <- data.frame(
  s = s_pred,
  value= beta_hat,
  lower = beta_hat_LB_global,
  upper = beta_hat_UB_global)

zero_older <- data.frame(
  s=0,
  value= zero_hat,
  lower = zero_hat_LB,
  upper = zero_hat_UB)

save(beta_older, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/beta_older.rda")
#save(zero_older, file = "C:/Users/jlopez/Desktop/analyses/data/all/15s/fda/coefficients/zero_older.rda")
