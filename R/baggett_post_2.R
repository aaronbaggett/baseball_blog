# ===========================================================
# R Code
# Exploring Baseball Data with R
# Post: Conceptualizing the MLB Strike Zone Using PITCHf/x Data Pt. 2
# 04/08/2015
# ===========================================================

# Load package libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

# Read in 2014 PITCHf/x data
load(url("http://aaronbaggett.com/data/pfx_14.rda"))
glimpse(pfx_14)

# Filter all Matt Carpenter 2014 at bats
carp <- pfx_14 %>% 
  #select(sz_top, sz_bot, player_sz_top, player_sz_bot) %>% 
  filter(batter_name == "Matt Carpenter")

# Shapiro-Wilk test of normality for
# Matt Carpenter's 2014 sz_top/sz_bot
shapiro.test(carp$sz_top)
shapiro.test(carp$sz_bot)

# Density plot of Matt Carpenter's 2014 sz_top
ggplot(data = carp, aes(x = sz_top)) + 
  geom_density(fill = "gray35") +
  scale_x_continuous(breaks = seq(0, 6, 0.10), 
    name = "\nTop of Strike Zone (sz_top)") +
  scale_y_continuous(limits = c(0, 8), 
    breaks = seq(0, 8, 1), name = "Density\n") +
  theme_bw()

# Density plot of Matt Carpenter's 2014 sz_bot
ggplot(data = carp, aes(x = sz_bot)) +
  geom_density(fill = "gray35") +
  scale_x_continuous(breaks = seq(0, 2, 0.10), 
    name = "\nBottom of Strike Zone (sz_bot)") +
  scale_y_continuous(limits = c(0, 16), 
    breaks = seq(0, 16, 2), name = "Density\n") +
  theme_bw()

# Read in raw ANSUR data
ansur <- read.delim("http://aaronbaggett.com/data/ansur_men.txt", header = TRUE)

# Select a few variables from ANSUR data
ansur <- ansur %>% 
<<<<<<< HEAD
  select(STATURE, SUPRASTERNALE_HT, WAIST_HT_NATURAL, PATELLA.MID_HT) %>% 
  mutate(UPPER_BOUND = WAIST_HT_NATURAL - (STATURE - PATELLA.MID_HT), LOWER_BOUND = PATELLA.MID_HT)
=======
  select(STATURE, WAIST_HT_NATURAL, PATELLA.MID_HT)

# Convert ANSUR dimensions from millimeters to inches
ansur <- compute(ansur*0.0393701)

# Round values
ansur$STATURE <- round(ansur$STATURE)

# Convert ANSUR data from inches to feet
ansur_hts <- ansur %>% 
  select(b_height = STATURE, WAIST_HT_NATURAL, PATELLA.MID_HT) %>% 
  group_by(b_height) %>%
  summarize(ht_top = mean(WAIST_HT_NATURAL)/12, ht_bott = mean(PATELLA.MID_HT)/12)

# Create *u_test_ht* variable for umpire's decision [1 = correct]
# given ANSUR data/batter height transformation
# Includes ball radius = ((1.57*2 + 17) / 12) / 2 = 0.8391667
pfx_14$u_test_ht <- with(pfx_14,
  ifelse(call == "Ball" & px < -0.8391667 | px > 0.8391667 | 
    pz < ht_bott | pz > ht_top, 1,
  ifelse(call == "Called Strike" & pz >= ht_bott & pz <= ht_top & 
    px >= -0.8391667 & px <= 0.8391667, 1,
  ifelse(call == "Ball" & pz >= ht_bott & pz <= ht_top & 
    px > -0.8391667 & px < 0.8391667, 0,
  ifelse(call == "Called Strike" & px < -0.8391667 | px > 0.8391667 | 
    pz < ht_bott | pz > ht_top, 0, 99)))))

## Calculate mean and se umpire accuracy rates
(pfx_accuracy <- pfx_14 %>%
    group_by(umpire) %>%
    summarize(accuracy = mean(u_test),
      se = sd(u_test) / sqrt(length(u_test)))
)

# Calculate mean *accuracy*
with(pfx_accuracy, mean(accuracy))
>>>>>>> origin/master

# Calculate sd *accuracy*
with(pfx_accuracy, sd(accuracy))

<<<<<<< HEAD
STATURE: height
SUPRASTERNALE_HT: length from top of shoulders to ground
WAIST_HT.OMPHALION: length from top of pants to ground
WAIST_HT_NATURAL: length from midpoint between shoulders and waist and ground
PATELLA.MID_HT: length from knee to ground
=======
# Sort *pfx_accuracy* in descending order
sort_pfx_accuracy <- pfx_accuracy[order(-pfx_accuracy$accuracy), ]
>>>>>>> origin/master

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
    name = "\nUmpire Decision Accuracy\nWidth of Strike Zone = 20.14 inches") +
  scale_y_discrete(name = "", labels = rev(sort_pfx_accuracy$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6))
