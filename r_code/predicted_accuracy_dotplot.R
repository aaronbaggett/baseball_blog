# ==========================================================================
# *m1:m6* Predicted Probabilities Dot Plot
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
load(url("http://aaronbaggett.com/data/m4.rda"))
load(url("http://aaronbaggett.com/data/m5.rda"))
load(url("http://aaronbaggett.com/data/m6.rda"))

### --- Inverse Logit Function --- ###
inv.logit <- function(x){
	1 / (1 + exp(-x))
}

### --- Standard error function --- ###
se <- function(x){
	sqrt(var(x) / sqrt(length(x)))
}

### --- Subset data for dot plotting --- ###
by_umpire <- pfx_13 %>%
  group_by(umpire, uni_num) %>%
  summarize(accuracy = mean(u_test), 
    se = sd(u_test) / sqrt(length(u_test)))

### --- Subset data for dot plotting --- ###
by_umpire <- pfx_13 %>%
  group_by(umpire, uni_num) %>%
  summarize(accuracy = mean(u_test), 
    se = sd(u_test) / sqrt(length(u_test)))

set.seed(1100)
id <- sample(1100:1175)

by_umpire$id <- id

# Sort *by_umpire in descending order
sort_by_umpire <- by_umpire[order(-by_umpire$accuracy), ]

m1_preds <- data.frame(umpire = with(pfx_13, unique(umpire)), u_test = 0)
mm <- model.matrix(terms(m1), m1_preds)
m1_preds$u_test <- predict(m1, m1_preds)

pvar1 <- diag(mm %*% tcrossprod(vcov(m1), mm))
tvar1 <- pvar1 + VarCorr(m1)$umpire[1]

m1_preds <- data.frame(m1_preds, 
  plo = m1_preds$u_test - 2*sqrt(pvar1),
  phi = m1_preds$u_test + 2*sqrt(pvar1),
  tlo = m1_preds$u_test - 2*sqrt(tvar1),
  thi = m1_preds$u_test + 2*sqrt(tvar1)
)

m1_preds[, 2:6] <- inv.logit(m1_preds[, 2:6])

m1_preds$id <- id

# Sort "dot.df" in descending order
m1_preds <- m1_preds[order(-m1_preds$u_test), ]
m1_preds$id <- sort_by_umpire$id

### --- Plot dot plot --- ###
ggplot(data = m1_preds, 
  aes(x = u_test, y = sort(umpire, decreasing = TRUE))) +
  #geom_line(aes(group = 1), color = "grey50") +
  geom_vline(aes(xintercept = mean(u_test)), 
    color = "gray30", linetype = 2, size = 0.35) +
  geom_segment(aes(x = u_test - abs(u_test - plo), xend = u_test + abs(u_test - phi), 
    y = sort(umpire, decreasing = TRUE), 
    yend = sort(umpire, decreasing = TRUE)), 
    color = "grey30", size = 0.25) + 
  geom_line(aes(group = 1), color = "gray30") +
  geom_point(color = "gray10") + 
  scale_x_continuous(limits = c(0.875, 0.955), breaks = seq(0.875, 0.955, 0.010), 
    name = "\nPredicted Umpire Decision Accuracy") +
  scale_y_discrete("Umpire ID\n", labels = rev(m1_preds$id)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())

