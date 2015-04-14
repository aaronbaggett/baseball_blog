# ===========================================================
# R Code
# Exploring Baseball Data with R
# Post: Title
# 02/16/2015
# ===========================================================

# Load package libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

# Read in 2014 PITCHf/x data
load(url("http://aaronbaggett.com/data/pfx_14.rda"))

# Find width of PITCHf/x parameters by *zone*
pfx_14 %>% 
  group_by(zone) %>% 
  filter(zone <= 9) %>% 
  summarize(min_px = min(px), 
    mean_px = mean(px), 
    max_px = max(px), 
    sd_px = sd(px))

# Set up PITCH/fx native strike zone regions data
strike_zones <- data.frame(
  x1 = rep(-1.5:0.5, each = 3),
  x2 = rep(-0.5:1.5, each = 3),
  y1 = rep(1.5:3.5, 3),
  y2 = rep(2.5:4.5, 3),
  z = factor(c(7, 4, 1, 8, 5, 2, 9, 6, 3))
)

# Plot example of PITCHf/x strike zone regions
ggplot() + 
  xlim(-3, 3) + xlab("") +
  ylim(0, 6) + ylab("") +
  geom_rect(data = strike_zones, 
    aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1, fill = z), color = "grey20") +
  geom_text(data = strike_zones, 
    aes(x = x1 + (x2 - x1)/2, y = y1 + (y2 - y1)/2, label = z), 
    size = 7, fontface = 2, color = I("grey20")) + 
  theme_bw() + theme(legend.position = "none")

(pfx_accuracy <- pfx_14 %>%
    group_by(umpire) %>%
    summarize(accuracy = mean(u_test_pfx),
      se = sd(u_test_pfx) / sqrt(length(u_test_pfx)))
)

# Calculate mean *accuracy*
with(pfx_accuracy, mean(accuracy))

# Calculate sd *accuracy*
with(pfx_accuracy, sd(accuracy))

# Sort *pfx_accuracy* in descending order
sort_pfx_accuracy <- pfx_accuracy[order(-pfx_accuracy$accuracy), ]

### --- Build dotplot --- ###
ggplot(data = sort_pfx_accuracy, 
  aes(x = accuracy, y = sort(umpire, decreasing = TRUE))) +
  geom_vline(aes(xintercept = mean(accuracy)), 
    color = "red", linetype = 2, size = 0.35) +
  geom_segment(aes(x = accuracy - se, xend = accuracy + se, 
    y = sort(umpire, decreasing = TRUE), 
    yend = sort(umpire, decreasing = TRUE)), 
    color = "gray30", size = 0.25) +
  geom_line(aes(group = 1), color = "gray30") +
  geom_point(color = "gray10") + 
  scale_x_continuous(limits = c(0.78, 0.95), breaks = seq(0.78, 0.95, 0.010), 
    name = "\nUmpire Decision Accuracy\nWidth of Strike Zone = 17 inches") +
  scale_y_discrete(name = "", labels = rev(sort_pfx_accuracy$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6))

# Specify home plate polygon coordinates
plate.x <- c(-0.70833, 0.70833, 0.70833, 0, -0.70833)
plate.y <- c(1.41666, 1.41666, 0.70833, 0, 0.70833)

# Draw home plate polygon
ggplot(data = NULL, aes(x = plate.x, y = plate.y)) + 
  geom_polygon(fill = 'white', color = 'black', 
    size = 1.5, lineend = 'square') +
  scale_x_continuous(limits = c(-1.5, 1.5), breaks = seq(-1.5, 1.5, .25), name = '') +
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, .25), name = '') +
  geom_point(data = NULL, aes(x = 0.815, y = 1.062), 
    size = 15, fill = 'gray90', color = 'black', shape = 21) + # baseball (R)
  geom_point(data = NULL, aes(x = -0.815, y = 1.062), 
    size = 15, fill = 'gray90', color = 'black', shape = 21) + # baseball (L)
  geom_point(data = NULL, aes(x = -0.815, y = 1.062)) + # point marking radius of baseball (L)
  geom_point(data = NULL, aes(x = 0.815, y = 1.062)) + # point marking radius of baseball (R)
  geom_segment(aes(x = -0.72, y = 1.55, xend = -0.15, yend = 1.55)) + # horizontal line from edge of plate to label (L)
  geom_segment(aes(x = 0.72, y = 1.55, xend = 0.15, yend = 1.55)) + # horizontal line from edge of plate to label (R)
  geom_segment(aes(x = -0.72, y = 1.47, xend = -0.72, yend = 1.62)) + # vertical line marking edge of plate label (L)
  geom_segment(aes(x = 0.72, y = 1.47, xend = 0.72, yend = 1.62)) + # vertical line marking edge of plate label (R)
  geom_segment(aes(x = -0.815, y = 0, xend = -0.815, yend = 1.81), linetype = 2) + # vertical dashed line through radius of ball (L)
  geom_segment(aes(x = 0.815, y = 0, xend = 0.815, yend = 1.81), linetype = 2) + # vertical dashed line through radius of ball (R)
  geom_segment(aes(x = -0.815, y = 1.062, xend = -0.70833, yend = 1.062), linetype = 2) + # # horizontal dashed line to edge of ball radius (L)
  geom_segment(aes(x = 0.815, y = 1.062, xend = 0.70833, yend = 1.062), linetype = 2) + # horizontal dashed line to edge of ball radius (R)
  geom_segment(aes(x = -0.815, y = 1.70, xend = -0.625, yend = 1.70), linetype = 2) + # horizontal line from radius of ball to label (L)
  geom_segment(aes(x = 0.815, y = 1.70, xend = 0.625, yend = 1.70), linetype = 2) + # horizontal line from radius of ball to label (R)
  annotate('text', x = 0, y = 1.55, label = '17.00') + # plate width label
  annotate('text', x = 0, y = 1.70, label = "~1.57*2 + 17.00 = ~20.14", parse = F) + # radius of baseball + plate width label
  theme_bw() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

# Create *u_test* variable for umpire's decision [1 = correct]
# Ball radius = ((1.57*2 + 17) / 12) / 2 = 0.8391667
pfx_14$u_test <- with(pfx_14,
  ifelse(call == "Ball" & px < -0.8391667 | px > 0.8391667 | 
      pz < player_sz_bot | pz > player_sz_top, 1,
    ifelse(call == "Called Strike" & pz >= player_sz_bot & pz <= player_sz_top & 
        px >= -0.8391667 & px <= 0.8391667, 1,
      ifelse(call == "Ball" & pz >= player_sz_bot & pz <= player_sz_top & 
          px > -0.8391667 & px < 0.8391667, 0,
        ifelse(call == "Called Strike" & px < -0.8391667 | px > 0.8391667 | 
            pz < player_sz_bot | pz > player_sz_top, 0, 99)))))

(new_pfx_accuracy <- pfx_14 %>%
    group_by(umpire) %>%
    summarize(accuracy = mean(u_test),
      se = sd(u_test) / sqrt(length(u_test)))
)

# Calculate mean *accuracy*
with(new_pfx_accuracy, mean(accuracy))

# Calculate sd *accuracy*
with(new_pfx_accuracy, sd(accuracy))

# Sort *pfx_accuracy* in descending order
sort_new_pfx_accuracy <- new_pfx_accuracy[order(-new_pfx_accuracy$accuracy), ]

### --- Build dotplot --- ###
ggplot(data = sort_new_pfx_accuracy, 
  aes(x = accuracy, y = sort(umpire, decreasing = TRUE))) +
  geom_vline(aes(xintercept = mean(accuracy)), 
    color = "red", linetype = 2, size = 0.35) +
  geom_segment(aes(x = accuracy - se, xend = accuracy + se, 
    y = sort(umpire, decreasing = TRUE), 
    yend = sort(umpire, decreasing = TRUE)), 
    color = "gray30", size = 0.25) +
  geom_line(aes(group = 1), color = "gray30") +
  geom_point(color = "gray10") + 
  scale_x_continuous(limits = c(0.85, 0.955), breaks = seq(0.85, 0.955, 0.010), 
    name = "\nUmpire Decision Accuracy\nWidth of Strike Zone = 20.14 inches") +
  scale_y_discrete(name = "", labels = rev(sort_pfx_accuracy$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6))

# Calculate mean and SD decisions umpires make during games?
decisions <- pfx_14 %>%
  group_by(umpire) %>%
  filter(call == "Called Strike" | call == "Ball") %>%
  summarize(games = length(unique(gameday_link)), 
    n = length(gameday_link), decisions = n/games)

decisions %>%
  summarize(mean = mean(decisions), sd = sd(decisions))







library(dplyr)
library(ggplot2)

load("~/Dropbox/Baseball With R/Data/pfx_14.rda")

pfx_14 <- tbl_df(pfx_14)

pfx_samp <- sample_n(pfx_14, 1000)

pfx_samp$h_dist <- with(pfx_samp, ifelse(px < 0, px - (-0.87),
  ifelse(px > 0, 0.87 - px, 99)))

pfx_samp$h_mid <- (pfx_samp$player_sz_top - pfx_samp$player_sz_bot) / 2
pfx_samp$h_mid <- pfx_samp$h_mid + pfx_samp$player_sz_bot

pfx_samp$v_dist <- with(pfx_samp, ifelse(pz < h_mid, player_sz_bot - pz, ifelse(pz > h_mid, player_sz_top - pz, 99)))

pfx_samp %>%
  summarize(mean_top = mean(player_sz_top), 
    mean_bot = mean(player_sz_bot))

if px is less than 0, then px minus left edge
if px is greater than 0, then right edge minus px
if pz is less than players middle boundry , then players lower boundry minus pz
if pz is greater than players middle boundry, then players upper boundry minus pz

The problem is some values will be positive and some will be negative
Should all distances outside of the strike zone be negative?
In other words, if px is greater than 0 and also greater than right edge, then right edge minus px.
Probably doesnt matter.

ggplot(data = pfx_samp, aes(x = px, y = pz)) + geom_point(color = "gray40") +
  scale_x_continuous(limits = c(-3, 3), breaks = seq(-3, 3, 1)) + 
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +
  geom_rect(xmin = -0.87, xmax = 0.87, ymin = 1.573159, ymax = 3.421909, 
    fill = NA, color = "salmon") +
  geom_segment(x = pfx_samp$px, xend = (ifelse(pfx_samp$px < 0, -0.87, 0.87)), 
    y = pfx_samp$pz, yend = pfx_samp$pz, color = "gray40") +
  geom_segment(x = -0.87, xend = -0.87, y = 0, yend = 5, color = "salmon") +
  geom_segment(x = 0.87, xend = 0.87, y = 0, yend = 5, color = "salmon") +
  theme_bw() + theme(legend.position = "none")

pfx_14 %>%
  filter(zone <= 9) %>%
  group_by(zone) %>%
  summarize(min_px = min(px), max_px = max(px))

### --- Create *u_test* variable for umpire's decision [1 = correct] --- ###
# Ball radius = ((1.57*2 + 17) / 12) / 2
pfx_14$u_test_pfx <- with(pfx_14,
  ifelse(call == "Ball" & px < -0.708 | px > 0.708 | 
      pz < player_sz_bot | pz > player_sz_top, 1,
  ifelse(call == "Called Strike" & pz > player_sz_bot & pz < player_sz_top & 
      px >= -0.708 & px <= 0.708, 1,
  ifelse(call == "Ball" & pz > player_sz_bot & pz < player_sz_top & 
      px > -0.708 & px < 0.708, 0,
  ifelse(call == "Called Strike" & px < -0.708 | px > 0.708 | 
      pz < player_sz_bot | pz > player_sz_top, 0, 99)))))

(17 / 12) / 2 = 0.7083333

with(pfx_14, mean(u_test))
with(pfx_14, mean(u_test_pfx))

