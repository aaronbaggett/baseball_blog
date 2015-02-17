# ==========================================================================
# *m1* Predicted Probabilities Dot Plot
# ==========================================================================

### --- Load pacakges (if necessary) --- ###
library(lme4, quietly = TRUE)
library(dplyr, quietly = TRUE)
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

### --- Find mean and sd calls made by umpires --- ###
pfx_13_sub <- pfx_13 %>%
  group_by(umpire) %>%
  filter(length(u_test) >= 1000) %>%
  summarize(mean = mean(u_test), sd = sd(u_test))

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

newdat <- data.frame(umpire = with(pfx_13, unique(umpire)), u_test = 0)
mm <- model.matrix(terms(m1), newdat)
newdat$u_test <- predict(m1, newdat)
newdat$u_test_prob <- inv.logit(newdat$u_test)

pvar1 <- diag(mm %*% tcrossprod(vcov(m1), mm))
tvar1 <- pvar1 + VarCorr(m1)$umpire[1]

newdat <- data.frame(newdat, 
  plo = newdat$u_test - 2*sqrt(pvar1),
  phi = newdat$u_test + 2*sqrt(pvar1),
  tlo = newdat$u_test - 2*sqrt(tvar1),
  thi = newdat$u_test + 2*sqrt(tvar1)
)

newdat[, 2:6] <- inv.logit(newdat[, 2:6])

set.seed(1100)
id <- sample(1100:1175)

newdat$id <- id

# Sort "dot.df" in descending order
newdat <- newdat[order(-newdat$u_test), ]
newdat$id <- sort_by_umpire$id

newdat %>%
  #group_by(umpire) %>%
  summarize(min = min(u_test), max = max(u_test), mean = mean(u_test), sd = sd(u_test))

### --- Plot dot plot --- ###
ggplot(data = newdat, 
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
  scale_y_discrete("Umpire ID\n", labels = rev(newdat$id)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())

