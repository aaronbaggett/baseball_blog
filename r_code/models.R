# ==========================================================================
# Multilevel Model Summary Code
# ==========================================================================

### --- Load pacakges (if necessary) --- ###
library(lme4, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(xtable)

### --- Read in multilevel data frame *pfx_13* --- ###
load(url("http://aaronbaggett.com/data/pfx_13.rda"))

### --- Relevel "zone_reg" to make "ball" reference group --- ###
pfx_13$zone_reg <- relevel(pfx_13$zone_reg, ref = "ball")

### --- Relevel "bs_count" to make "neutral" reference group --- ###
pfx_13$bs_count <- relevel(pfx_13$bs_count, ref = "neutral")

### --- Read in saved model output --- ###
load(url("http://aaronbaggett.com/data/m0.rda"))
load(url("http://aaronbaggett.com/data/m1.rda"))
load(url("http://aaronbaggett.com/data/m2.rda"))
load(url("http://aaronbaggett.com/data/m3.rda"))
load(url("http://aaronbaggett.com/data/m3a.rda"))
load(url("http://aaronbaggett.com/data/m3b.rda"))
load(url("http://aaronbaggett.com/data/m3c.rda"))
load(url("http://aaronbaggett.com/data/m3d.rda"))
load(url("http://aaronbaggett.com/data/m4.rda"))
load(url("http://aaronbaggett.com/data/m5.rda"))
load(url("http://aaronbaggett.com/data/m6.rda"))

### --- Inverse Logit Function --- ###
inv.logit <- function(x){
	1 / (1 + exp(-x))
}

### --- Standard error function --- ###
se <- function(var, length){
	sqrt(var) / sqrt(length)
}

se <- function(x){
	sqrt(var(x)) / sqrt(length(x))
}

# ==========================================================================
# Null Model, *m0*
#
# m0 <- glm(u_test ~ 1, data = pfx_14, family = binomial)
# 
# ==========================================================================
summary(m0)

coef <- m0$coefficients
exp(coef) / (1 + exp(coef))

11.46 / (1 + 11.46)
11.74 / (1 + 11.74)

or <- exp(2.451089)
inv.logit(or)

exp_ci <- exp(confint(m0))
prob_exp_ci <- inv.logit(exp_ci)

# ==========================================================================
# Fully Unconditional Model, *m1*
#
# m1 <- glmer(u_test ~ 1 + (1 | umpire), data = pfx_13,
#   family = binomial, nAGQ = 0)
#
# ==========================================================================
summary(m1)

# Extract umpire variance
m1_aic <- summary(m1)$AICtab
m1_vc <- VarCorr(m1)
print(m1_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m1_coeffs <- summary(m1)$coefficients
rr0 <- ranef(m1, condVar = TRUE)
m1_resids <- residuals(m1)

# Transform m1 fixed effects
# 1. Extract fixed effects
(m1_fe <- fixef(m1))

# 2. Obtain odds ratios
(m1_odds <- exp(m1_fe))

# 3. Obtain probability
(m1_prob <- inv.logit(m1_fe))

# 4. Calculate m1 ICC
# 4.1 Extract residual variance
m1_var <- summary(m1)$varcor$umpire[1, 1]
(ump_icc <- m1_var / (m1_var + (pi^2) / 3))

# 5. m1 fixed effect CIs
m1_se <- sqrt(diag(vcov(m1)))
(m1_cis <- data.frame(Est = fixef(m1), 
  LL = fixef(m1) - 1.96 * m1_se, 
  UL = fixef(m1) + 1.96 * m1_se))
(m1_cis_exp <- exp(m1_cis))

# 6. m1 random effect CIs
m1_s2 <- m1@theta
m1_n <- nrow(ranef(m1)$umpire)
(m1_sd_cis <- data.frame(Est = m1_s2,
  LL = (m1_n - 1) * m1_s2 / qchisq(0.975, df = m1_n - 1),
  UL = (m1_n - 1) * m1_s2 / qchisq(0.025, df = m1_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m1_cis, "REs:" = m1_sd_cis)

# 8. Calculate design effect (Peugh, 2010)
# 8.1 Obtain sample size
(samp.size <- nrow(pfx_13))
# 5.2 Obtain number of umpires
(n.umps <- length(unique(pfx_13$umpire)))
# 5.3 Calculate design effect
(des.eff <- 1 + (((samp.size / n.umps) - 1) * ump_icc))


# ==========================================================================
# Means as Outcomes Regression Model, *m2*
#
# m2 <- glmer(u_test ~ scale(yr_exp) + (1 | umpire), data = pfx_13, 
#   family = binomial, nAGQ = 0)
#
# ==========================================================================
summary(m2)

m2.preds <- predict(m2, type = "response")

# Extract random variance
m2_aic <- summary(m2)$AICtab
m2_vc <- VarCorr(m2)
print(m2_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m2_var <- summary(m2)$varcor$umpire[1, 1]

m2_coeffs <- summary(m2)$coefficients
rr2 <- ranef(m2, condVar = TRUE)
rr2$umpire$"factor(bs_count)batter"
m2_resids <- residuals(m2)

# Transform m2 fixed effects
# 1. Extract fixed effects
(m2_fe <- fixef(m2))

# 2. Obtain odds ratios
(m2_odds <- exp(m2_fe))

# 3. Obtain probability
(m2_prob <- inv.logit(m2_fe))

# 4. m2 FE CIs
m2_se <- sqrt(diag(vcov(m2)))
(m2_cis <- data.frame(Est = fixef(m2), 
  LL = fixef(m2) - 1.96 * m2_se, 
  UL = fixef(m2) + 1.96 * m2_se))
(m2_cis_exp <- exp(m2_cis))

# 5. m2 RE CIs
m2_s2 <- m2@theta
m2_n <- nrow(ranef(m2)$umpire)
(m2_sd_cis <- data.frame(Est = m2_s2,
  LL = (m2_n - 1) * m2_s2 / qchisq(0.975, df = m2_n - 1),
  UL = (m2_n - 1) * m2_s2 / qchisq(0.025, df = m2_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m2_cis, "REs:" = m2_sd_cis)


# ==========================================================================
# One-Way ANCOVA with Random Effects, *m3*
# 
# m3 <- glmer(u_test ~ factor(zone_reg) + factor(bs_count) + (1 | umpire), 
#   data = pfx_13, family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m3)

# Extract random variance
m3_aic <- summary(m3)$AICtab
m3_vc <- VarCorr(m3)
m3_df <- print(m3_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m3_coeffs <- summary(m3)$coefficients
rr3 <- ranef(m3, condVar = TRUE)
m3_resids <- residuals(m3)
xtable(m3_coeffs, digits = 3, caption = "Here is the caption", label = "table:m3")

# Transform m3 fixed effects
# 1. Extract fixed effects
(m3_fe <- fixef(m3))

# 2. Obtain odds ratios
(m3_odds <- exp(m3_fe))

# 3. Obtain probability
(m3_prob <- inv.logit(m3_fe))

# 4. m3 FE CIs
m3_se <- sqrt(diag(vcov(m3)))
(m3_cis <- data.frame(Est = fixef(m3), 
  LL = fixef(m3) - 1.96 * m3_se, 
  UL = fixef(m3) + 1.96 * m3_se))
(m3_cis_exp <- exp(m3_cis))

# 5. m3 RE CIs
m3_s2 <- m3@theta
m3_n <- nrow(ranef(m3)$umpire)
(m3_sd_cis <- data.frame(Est = m3_s2,
  LL = (m3_n - 1) * m3_s2 / qchisq(0.975, df = m3_n - 1),
  UL = (m3_n - 1) * m3_s2 / qchisq(0.025, df = m3_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m3_cis, "REs:" = m3_sd_cis)

# ==========================================================================
# One-Way ANCOVA with Random Effects, *m3a*
# 
# m3a <- glmer(u_test ~ factor(zone_reg) + (1 | umpire), 
#  data = pfx_13, family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m3a)

# Extract random variance
m3a_aic <- summary(m3a)$AICtab
m3a_vc <- VarCorr(m3a)
m3a_df <- print(m3a_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m3a_coeffs <- summary(m3a)$coefficients
rr3a <- ranef(m3a, condVar = TRUE)
m3a_resids <- residuals(m3a)
xtable(m3a_coeffs, digits = 3, caption = "Here is the caption", label = "table:m3a")

# Transform m3a fixed effects
# 1. Extract fixed effects
(m3a_fe <- fixef(m3a))

# 2. Obtain odds ratios
(m3a_odds <- exp(m3a_fe))

# 3. Obtain probability
(m3a_prob <- inv.logit(m3a_fe))

# 4. m3a FE CIs
m3a_se <- sqrt(diag(vcov(m3a)))
(m3a_cis <- data.frame(Est = fixef(m3a), 
  LL = fixef(m3a) - 1.96 * m3a_se, 
  UL = fixef(m3a) + 1.96 * m3a_se))
(m3a_cis_exp <- exp(m3a_cis))

# 5. m3a RE CIs
m3a_s2 <- m3a@theta
m3a_n <- nrow(ranef(m3a)$umpire)
(m3a_sd_cis <- data.frame(Est = m3a_s2,
  LL = (m3a_n - 1) * m3a_s2 / qchisq(0.975, df = m3a_n - 1),
  UL = (m3a_n - 1) * m3a_s2 / qchisq(0.025, df = m3a_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m3a_cis, "REs:" = m3a_sd_cis)


# ==========================================================================
# One-Way ANCOVA with Random Effects, *m3b*
# 
# m3b <- glmer(u_test ~ factor(bs_count) + (1 | umpire), 
#  data = pfx_13, family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m3b)

# Extract random variance
m3b_aic <- summary(m3b)$AICtab
m3b_vc <- VarCorr(m3b)
m3b_df <- print(m3b_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m3b_coeffs <- summary(m3b)$coefficients
rr3ba <- ranef(m3b, condVar = TRUE)
m3b_resids <- residuals(m3b)
xtable(m3b_coeffs, digits = 3, caption = "Here is the caption", label = "table:m3b")

# Transform m3b fixed effects
# 1. Extract fixed effects
(m3b_fe <- fixef(m3b))

# 2. Obtain odds ratios
(m3b_odds <- exp(m3b_fe))

# 3. Obtain probability
(m3b_prob <- inv.logit(m3b_fe))

# 4. m3b FE CIs
m3b_se <- sqrt(diag(vcov(m3b)))
(m3b_cis <- data.frame(Est = fixef(m3b), 
  LL = fixef(m3b) - 1.96 * m3b_se, 
  UL = fixef(m3b) + 1.96 * m3b_se))
(m3b_cis_exp <- exp(m3b_cis))

# 5. m3b RE CIs
m3b_s2 <- m3b@theta
m3b_n <- nrow(ranef(m3b)$umpire)
(m3b_sd_cis <- data.frame(Est = m3b_s2,
  LL = (m3b_n - 1) * m3b_s2 / qchisq(0.975, df = m3b_n - 1),
  UL = (m3b_n - 1) * m3b_s2 / qchisq(0.025, df = m3b_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m3b_cis, "REs:" = m3b_sd_cis)

0.18/0.00

# ==========================================================================
# One-Way ANCOVA with Random Effects, *m3c*
# 
# Model 3c
#
# m3c <- glmer(u_test ~ factor(zone_reg)*factor(bs_count) + (1 | umpire), data = pfx_13,
#  family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m3c)

# Extract random variance
m3c_aic <- summary(m3c)$AICtab
m3c_vc <- VarCorr(m3c)
m3c_df <- print(m3c_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m3c_coeffs <- summary(m3c)$coefficients
rr3ba <- ranef(m3c, condVar = TRUE)
m3c_resids <- residuals(m3c)
xtable(m3c_coeffs, digits = 3, caption = "Here is the caption", label = "table:m3c")

# Transform m3c fixed effects
# 1. Extract fixed effects
(m3c_fe <- fixef(m3c))

# 2. Obtain odds ratios
(m3c_odds <- exp(m3c_fe))

# 3. Obtain probability
(m3c_prob <- inv.logit(m3c_fe))

# 4. m3c FE CIs
m3c_se <- sqrt(diag(vcov(m3c)))
(m3c_cis <- data.frame(Est = fixef(m3c), 
  LL = fixef(m3c) - 1.96 * m3c_se, 
  UL = fixef(m3c) + 1.96 * m3c_se))
(m3c_cis_exp <- exp(m3c_cis))

# 5. m3c RE CIs
m3c_s2 <- m3c@theta
m3c_n <- nrow(ranef(m3c)$umpire)
(m3c_sd_cis <- data.frame(Est = m3c_s2,
  LL = (m3c_n - 1) * m3c_s2 / qchisq(0.975, df = m3c_n - 1),
  UL = (m3c_n - 1) * m3c_s2 / qchisq(0.025, df = m3c_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m3c_cis, "REs:" = m3c_sd_cis)


# ==========================================================================
# One-Way ANCOVA with Random Effects, *m3d*
# 
# Model 3d
#
# m3d <- glmer(u_test ~ factor(zone_reg)*factor(bs_count) + (factor(zone_reg) + 
#  factor(bs_count) | umpire), data = pfx_13, family = binomial, nAGQ = 0)
# ==========================================================================
summary(m3d)

# Extract random variance
m3d_aic <- summary(m3d)$AICtab
m3d_vc <- VarCorr(m3d)
m3d_vc <- as.data.frame(VarCorr(m3d))
names(m3d_vc) <- c("Group", "V1", "V2", "VCov", "SD/Cor")

m3d_vc$V1[7:21] <- c(rep("Intercept", 5), rep("Inner", 4), rep("Middle", 3), rep("Outer", 2), "Batter")
m3d_vc$V2[7:21] <- c("Inner", "Middle", "Outer", "Batter", "Pitcher", "Middle", "Outer", "Batter", "Pitcher", "Outer", "Batter", "Pitcher", "Batter", "Pitcher", "Pitcher")

xtable(m3d_vc[7:21, -1])

# Extract random components for variance/covariance matrix
vars <- m3d_vc[1:6, 4]
corrs <- m3d_vc[7:21, 4]
covs <- m3d_vc[7:21, 5]

covar <- data.frame(corrs, covs)

mat <- matrix(1:36, 6, 6)
mat[lower.tri(mat)] <- covs
mat[upper.tri(mat)] <- corrs
mat[diag(mat)] <- vars

car:::Anova.mer(m3d)

m3d_df <- print(m3d_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m3d_coeffs <- summary(m3d)$coefficients
rr3ba <- ranef(m3d, condVar = TRUE)
m3d_resids <- residuals(m3d)
xtable(m3d_coeffs, digits = 3, caption = "Here is the caption", label = "table:m3d")

# Transform m3d fixed effects
# 1. Extract fixed effects
(m3d_fe <- fixef(m3d))

# 2. Obtain odds ratios
(m3d_odds <- exp(m3d_fe))

# 3. Obtain probability
(m3d_prob <- inv.logit(m3d_fe))

# 4. m3d FE CIs
m3d_se <- sqrt(diag(vcov(m3d)))
(m3d_cis <- data.frame(Est = fixef(m3d), 
  LL = fixef(m3d) - 1.96 * m3d_se, 
  UL = fixef(m3d) + 1.96 * m3d_se))
(m3d_cis_exp <- exp(m3d_cis))
inv.logit(m3d_cis_exp)

row.names(m3d_cis_exp) <- c("Intercept", "Inner", "Middle", "Outer", "Batter", "Pitcher", "Inner x Batter", "Middle x Batter", "Outer x Batter", "Inner x Pitcher", "Middle x Pitcher", "Outer x Pitcher")

ors <- data.frame(var = row.names(m3d_cis_exp), or = m3d_cis_exp$Est)
ors <- ors[-1, ]
ors <- ors[-6:-11, ]

# Model 3d fixed effect predicted probabilities
ors %>%
  group_by(var) %>%
  summarize(pred.prob = (9.12*or) / (1 + 9.12*or))

ggplot(data = ors, aes(x = var, y = or, group = 1)) + geom_point() + geom_line()

# 5. m3d RE CIs
(m3d_s2 <- m3d@theta)
m3d_n <- nrow(ranef(m3d)$umpire)
(m3d_sd_cis <- data.frame(Est = m3d_s2,
  LL = (m3d_n - 1) * m3d_s2 / qchisq(0.975, df = m3d_n - 1),
  UL = (m3d_n - 1) * m3d_s2 / qchisq(0.025, df = m3d_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m3d_cis, "REs:" = m3d_sd_cis)

# ==========================================================================
# Random Coefficients Model, *m4*
# 
# m4 <- glmer(u_test ~ factor(zone_reg) + factor(bs_count) + 
#   (factor(zone_reg) + factor(bs_count) | umpire), data = pfx_13, 
#   family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m4)

# Extract random variance
m4_aic <- summary(m4)$AICtab
m4_vc <- VarCorr(m4)
m4_df <- print(m4_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m4_coeffs <- summary(m4)$coefficients
rr4 <- ranef(m4, condVar = TRUE)
rr4$umpire$"factor(zone_reg)inner"
rr4$umpire$"factor(zone_reg)middle"
rr4$umpire$"factor(zone_reg)outer"
rr4$umpire$"factor(bs_count)batter"
rr4$umpire$"factor(bs_count)pitcher"

m4_resids <- residuals(m4)

# Transform m4 fixed effects
# 1. Extract fixed effects
(m4_fe <- fixef(m4))

# 2. Obtain odds ratios
(m4_odds <- exp(m4_fe))

# 3. Obtain probability
(m4_prob <- inv.logit(m4_fe))

# 4. m4 FE CIs
m4_se <- sqrt(diag(vcov(m4)))
(m4_cis <- data.frame(Est = fixef(m4), 
  LL = fixef(m4) - 1.96 * m4_se, 
  UL = fixef(m4) + 1.96 * m4_se))
(m4_cis_exp <- exp(m4_cis))

# 5. m4 RE CIs
m4_df <- as.data.frame(VarCorr(m4))
names(m4_df)
m4_df[1:6, 5]

m4_s2 <- m4_df[1:6, 5]
m4_n <- nrow(ranef(m4)$umpire)
(m4_sd_cis <- data.frame(Est = m4_s2,
  LL = (m4_n - 1) * m4_s2 / qchisq(0.975, df = m4_n - 1),
  UL = (m4_n - 1) * m4_s2 / qchisq(0.025, df = m4_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m4_cis, "REs:" = m4_sd_cis)


# ==========================================================================
# Intercepts and Slopes as Outcomes Model, *m5*
# 
# m5 <- glmer(u_test ~ factor(zone_reg) + factor(bs_count) + scale(yr_exp) + 
#   (factor(zone_reg) + factor(bs_count) | umpire), data = pfx_13, 
#   family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m5)

# Extract random variance
m5_aic <- summary(m5)$AICtab
m5_vc <- VarCorr(m5)
m5_df <- print(m5_vc, comp = c("Variance", "Std.Dev"), digits = 3)

m5_coeffs <- summary(m5)$coefficients
rr5 <- ranef(m5, condVar = TRUE)
rr5$umpire$"factor(zone_reg)inner"
rr5$umpire$"factor(zone_reg)middle"
rr5$umpire$"factor(zone_reg)outer"
rr5$umpire$"factor(bs_count)batter"
rr5$umpire$"factor(bs_count)pitcher"
m5_resids <- residuals(m5)

# Transform m5 fixed effects
# 1. Extract fixed effects
(m5_fe <- fixef(m5))

# 2. Obtain odds ratios
(m5_odds <- exp(m5_fe))

# 3. Obtain probability
(m5_prob <- inv.logit(m5_fe))

# 4. m5 FE CIs
m5_se <- sqrt(diag(vcov(m5)))
(m5_cis <- data.frame(Est = fixef(m5), 
  LL = fixef(m5) - 1.96 * m5_se, 
  UL = fixef(m5) + 1.96 * m5_se))
(m5_cis_exp <- exp(m5_cis))

# 5. m5 RE CIs
m5_df <- as.data.frame(VarCorr(m5))
names(m5_df)
m5_df[1:6, 5]

m5_s2 <- m5_df[1:6, 5]
m5_n <- nrow(ranef(m5)$umpire)
(m5_sd_cis <- data.frame(Est = m5_s2,
  LL = (m5_n - 1) * m5_s2 / qchisq(0.975, df = m5_n - 1),
  UL = (m5_n - 1) * m5_s2 / qchisq(0.025, df = m5_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m5_cis, "REs:" = m5_sd_cis)

# ==========================================================================
# Intercepts and Slopes as Outcomes with Cross-Level Interactions, *m6*
# 
# m6 <- glmer(u_test ~ factor(zone_reg) + 
#   factor(bs_count) + scale(yr_exp) + 
#   scale(yr_exp)*factor(zone_reg) + 
#   scale(yr_exp)*factor(bs_count) + 
#   (1 | umpire), data = pfx_13, family = binomial, nAGQ = 0)
#
# m6 <- glmer(u_test ~ factor(zone_reg)*scale(yr_exp) + 
#   factor(bs_count)*scale(yr_exp) + 
#   (1 | umpire), data = pfx_13, family = binomial, nAGQ = 0)
# 
# ==========================================================================
summary(m6, correlation = FALSE)

# Extract random variance
m6_aic <- summary(m6)$AICtab
m6_vc <- as.data.frame(VarCorr(m3d))
names(m6_vc) <- c("Group", "V1", "V2", "VCov", "SD/Cor")
xtable(m6_vc[, -1])

# Extract random components for variance/covariance matrix
vars <- m6_vc[1:6, 4]
corrs <- m6_vc[7:21, 4]
covs <- m6_vc[7:21, 5]

mat <- matrix(1:36, 6, 6)
mat[lower.tri(mat)] <- covs
mat[upper.tri(mat)] <- corrs
mat[diag(mat)] <- vars

xtable(mat)

car:::Anova.mer(m6)

m6_coeffs <- summary(m6)$coefficients
rr5 <- ranef(m6, condVar = TRUE)
rr5$umpire$"factor(bs_count)batter"
m6_resids <- residuals(m6)

# Transform m6 fixed effects
# 1. Extract fixed effects
(m6_fe <- fixef(m6))

# 2. Obtain odds ratios
(m6_odds <- exp(m6_fe))

# 3. Obtain probability
(m6_prob <- inv.logit(m6_fe))

# 4. m6 FE CIs
m6_se <- sqrt(diag(vcov(m6)))
(m6_cis <- data.frame(Est = fixef(m6), 
  LL = fixef(m6) - 1.96 * m6_se, 
  UL = fixef(m6) + 1.96 * m6_se))
(m6_cis_exp <- exp(m6_cis))

# 5. m6 RE CIs
m6_df <- as.data.frame(VarCorr(m6))
names(m6_df)

m6_s2 <- m6_df[1:6, 5]
m6_n <- nrow(ranef(m6)$umpire)
(m6_sd_cis <- data.frame(Est = m6_s2,
  LL = (m6_n - 1) * m6_s2 / qchisq(0.975, df = m6_n - 1),
  UL = (m6_n - 1) * m6_s2 / qchisq(0.025, df = m6_n - 1)))

# 7. Rbind FE/RE CIs
rbind("FEs:" = m6_cis, "REs:" = m6_sd_cis)

# ==========================================================================
# Model Rankings
# ==========================================================================
pfx_models <- list( )
pfx_models[[1]] <- m0
pfx_models[[2]] <- m1
pfx_models[[3]] <- m2
pfx_models[[4]] <- m3
pfx_models[[5]] <- m3a
pfx_models[[6]] <- m3b
pfx_models[[7]] <- m3c
pfx_models[[8]] <- m3d
pfx_models[[9]] <- m4
pfx_models[[10]] <- m5
pfx_models[[11]] <- m6

model.names <- c("Model 0", "Model 1", "Model 2", "Model 3", 
  "Model 3a", "Model 3b", "Model 3c", "Model 3d", "Model 4", "Model 5", "Model 6")
names(pfx_models) <- model.names
results <- data.frame(models = model.names)
results$bic.val <- unlist(lapply(pfx_models, BIC))
results$bic.rank <- rank(results$bic.val)
results$aic.val <- unlist(lapply(pfx_models, AIC))
results$aic.delta <- results$aic.val-min(results$aic.val)
results$aic.likelihood <-  exp(-0.5* results$aic.delta)
results$aic.weight <- results$aic.likelihood/sum(results$aic.likelihood)
results <- results[rev(order(results[, "aic.weight"])), ]
results$cum.aic.weight <- cumsum(results[, "aic.weight"])
results

xtable(results)
