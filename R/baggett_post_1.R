# ===========================================================
# R Code
# Exploring Baseball Data with R
# Post: Title
# 02/16/2015
# ===========================================================

# Load package libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lme4, quietly = TRUE)

# Read in 2014 PITCHf/x data
load(url("http://aaronbaggett.com/data/pfx_14.rda"))
glimpse(pfx_14)

pfx_14 <- tbl_df(pfx_14)
pfx_samp <- sample_n(pfx_14, 100)

(pfx_samp <- pfx_samp %>% 
    select(gameday_link, zone, sz_top, sz_bot, px, pz, call))

# Create *u_test_pfx* variable for umpire's decision [1 = correct]
pfx_14$u_test_pfx <- with(pfx_14,
  ifelse(call == "Ball" & zone > 9, 1,
  ifelse(call == "Called Strike" & zone <= 9, 1,
  ifelse(call == "Ball" & zone <= 9, 0,
  ifelse(call == "Called Strike" & zone > 9, 0, 99)))))

with(pfx_14, table(u_test_pfx))
with(pfx_14, mean(u_test_pfx))

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

with(pfx_14, table(u_test))
with(pfx_14, mean(u_test))

# Find width of pfx parameters
# RHB
pfx_14 %>% 
  group_by(zone) %>% 
  filter(zone <= 9) %>% 
  filter(stand == "R") %>% 
  summarize(min_px = min(px), 
    mean_px = mean(px), 
    max_px = max(px), 
    sd_px = sd(px))

# LHB
pfx_14 %>% 
  group_by(zone) %>% 
  filter(zone <= 9) %>% 
  filter(stand == "L") %>% 
  summarize(min_px = min(px), 
    mean_px = mean(px), 
    max_px = max(px), 
    sd_px = sd(px))

x <- pfx_14 %>%
  group_by(umpire) %>%
  summarize(zone_width = mean(u_test))

y <- pfx_14 %>% 
  group_by(umpire) %>% 
  summarize(plate_width = mean(u_test_pfx))

z <- inner_join(x, y, by = "umpire")
z$width_diff <- with(z, zone_width - plate_width)

# How many games does an umpire work behind the plate in a single season?
games <- pfx_14 %>%
  group_by(umpire) %>%
  #filter(length(call) > 1000) %>%
  summarize(games = length(unique(gameday_link)))

games %>%
  summarize(mean = mean(games), sd = sd(games))

# How many pitches do umpires view during the course a single game?
pitches <- pfx_14 %>%
  group_by(umpire) %>%
  #filter(length(call) > 1000) %>%
  summarize(games = length(unique(gameday_link)), 
    n = length(gameday_link), pitches = n/games)

pitches %>%
  summarize(mean_n = mean(n), sd_n = sd(n), 
  mean = mean(pitches), sd = sd(pitches))

# What's the mean and SD decisions umires make during games?
decisions <- pfx_14 %>%
  group_by(umpire) %>%
  #filter(length(call) > 1000) %>%
  filter(call == "Called Strike" | call == "Ball") %>%
  summarize(games = length(unique(gameday_link)), 
    n = length(gameday_link), decisions = n/games)

decisions %>%
  summarize(mean_n = mean(n), sd_n = sd(n), 
  mean = mean(decisions), sd = sd(decisions))

decisions %>%
  summarize(mean = mean(decisions), sd = sd(decisions))

experience <- pfx_14 %>%
  group_by(umpire) %>%
  summarize(exp = mean(yr_exp))

experience %>%
  summarize(min = min(exp), max = max(exp), 
    mean = mean(exp), sd = sd(exp))
