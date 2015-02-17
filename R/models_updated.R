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
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m0.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m1.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m2.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m3.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m4.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m4a.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m4b.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m5.rda"))
load(url("http://aaronbaggett.com/data/Updated_Output_Data/m6.rda"))

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


# ==============================================================
# Model 0
# 
# Empty logistic regression model for predicting probability of
# accurate umpire decision
# ==============================================================

m0 <- glm(u_test ~ 1, data = pfx_13, family = binomial)

# Print model output
summary(m0)

# Extract Model 0 coefficients
coef <- m0$coefficients

# Calculate odds ratio for Model 0
exp_coef <- exp(coef)

# Obtain 95% CI for odds ratio
exp_ci <- exp(confint(m0))

# Calculate inverse logit of exponentiated coefficient
ll <- exp_ci[1] / (1 + exp_ci[1])
ul <- exp_ci[2] / (1 + exp_ci[2])

# ==============================================================
# Model 1
#
# Predcits probability of accurate umpire decision with
# random effect for umpire
# ==============================================================

m1 <- glmer(u_test ~ 1 + (1 | umpire), data = pfx_13,
  family = binomial, nAGQ = 0)

# Extract umpire variance
m1_vc <- VarCorr(m1)
print(m1_vc, comp = c("Variance", "Std.Dev"), digits = 3)

# Transform m1 fixed effects
# 1. Extract fixed effects
(m1_fe <- fixef(m1))

# 2. Obtain odds ratios
(m1_odds <- exp(m1_fe))

# 3. Obtain probability
(m1_prob <- inv.logit(m1_fe))

# 4 Extract residual variance
m1_var <- summary(m1)$varcor$umpire[1, 1]

# 4.1 Calculate m1 ICC
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

# ==============================================================
# Model 2 was m3a
#
# Predicts probability of accurate umpire decision with fixed
# effects for pitch location (*zone_reg*), with random effects
# for umpire
# ==============================================================
 
m2 <- glmer(u_test ~ factor(zone_reg) + (1 | umpire), 
  data = pfx_13, family = binomial, nAGQ = 0)

# ==============================================================
# Model 3 was m3b
#
# Predicts probability of accurate umpire decision with fixed
# effects for ball-strike count (*bs_count*), with random effects
# for umpire
# ==============================================================

m3 <- glmer(u_test ~ factor(bs_count) + (1 | umpire), 
  data = pfx_13, family = binomial, nAGQ = 0)

# ==============================================================
# Model 4 was m3
#
# Predicts probability of accurate umpire decision with fixed
# effects for pitch location (*zone_reg*) and ball-strike count 
# (*bs_count*), with random effects for umpire
# ==============================================================

m4 <- glmer(u_test ~ factor(zone_reg) + factor(bs_count) + (1 | umpire), 
  data = pfx_13, family = binomial, nAGQ = 0)

# ==============================================================
# Model 4a was m3c
#
# Predicts probability of accurate umpire decision with fixed
# effects and interactions for pitch location (*zone_reg*) 
# and ball-strike count (*bs_count*), with random effects for 
# umpire
# ==============================================================

m4a <- glmer(u_test ~ factor(zone_reg)*factor(bs_count) + 
  (1 | umpire), data = pfx_13,
  family = binomial, nAGQ = 0)


# ==============================================================
# Model 4b was m3d
#
# Predicts probability of accurate umpire decision with fixed
# effects and interactions for pitch location (*zone_reg*) 
# and ball-strike count (*bs_count*), with random effects for 
# pitch location, ball-strike count, and umpire
# ==============================================================

m4b <- glmer(u_test ~ factor(zone_reg)*factor(bs_count) + 
       (factor(zone_reg) + factor(bs_count) | umpire), 
       data = pfx_13, family = binomial, nAGQ = 0)

# Extract random components for variance/covariance table
m4b_vc <- as.data.frame(VarCorr(m4b))
names(m4b_vc) <- c("Group", "V1", "V2", "VCov", "SD/Cor")
xtable(m4b_vc[, -1])

# Model 3d predicted probabilities table
pfx_13.pred <- pfx_13
X <- model.matrix(terms(m3d), data = pfx_13.pred)
b <- fixef(m4b)
pred.logit <- X %*% b

pred.prob  <- logit(pred.logit, inverse = TRUE)

pfx_13.pred2 <- data.frame(cbind("pred.logit" = pred.logit, 
  "pred.prob" = pred.prob, zone = pfx_13.pred$zone_reg, 
  count = pfx_13.pred$bs_count))

pfx_13.pred2$zone <- as.factor(pfx_13.pred2$zone)
levels(pfx_13.pred2$zone) <- c("Ball", "Inner", "Middle", "Outer")

pfx_13.pred2$count <- as.factor(pfx_13.pred2$count)
levels(pfx_13.pred2$count) <- c("Neutral", "Batter", "Pitcher")

colnames(pfx_13.pred2)[1:2] <- c("pred.logit","pred.prob")
(pred_prob_table <- unique(pfx_13.pred2[order(pfx_13.pred2$zone, 
  pfx_13.pred2$count), ])
)

# ==============================================================
# Model 5 was m2
#
# Predicts probability of accurate umpire decision with grand-mean
# centered fixed effect for umpire experience and random effect
# for umpire
# ==============================================================

m5 <- glmer(u_test ~ scale(yr_exp) + (1 | umpire), data = pfx_13, 
  family = binomial, nAGQ = 0)

# ==============================================================
# Model 6
#
# Predicts probability of accurate umpire decision with fixed
# effects for pitch location (*zone_reg*) and ball-strike count
# (*bs_count*), with interactions between pitch location, ball-
# strike count and umpire experience (*yr_exp*) with random 
# effect for umpire
# ==============================================================

m6 <- glmer(u_test ~ factor(zone_reg)*scale(yr_exp) + 
  factor(bs_count)*scale(yr_exp) + 
  (1 | umpire), data = pfx_13, family = binomial, nAGQ = 0)
