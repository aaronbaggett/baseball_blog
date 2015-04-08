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

heights <- read.csv("/Users/AB/Dropbox/Dissertation/Data/Anthropometric Data/Anthropometric_Measurements.csv")



WAIST_HT_NATURAL: length from ground to midpoint between shoulders and waist
PATELLA.MID_HT: length from ground to knee

feet <- as.numeric(as.character(substr(pfx_14$b_height, 1, 1)))*12
feet <- feet + as.numeric(as.character(substr(pfx_14$b_height, 3, 3)))

pfx_14$b_height <- feet

ansur <- read.delim("~/baseball_blog/data/ansur_men.txt", header = TRUE)

ansur <- ansur %>% 
  select(STATURE, SUPRASTERNALE_HT, WAIST_HT_NATURAL, PATELLA.MID_HT)

ansur <- compute(ansur*0.0393701)

ansur$STATURE <- round(ansur$STATURE)

ansur_hts <- ansur %>% 
  select(b_height = STATURE, WAIST_HT_NATURAL, PATELLA.MID_HT) %>% 
  group_by(b_height) %>%
  summarize(ht_top = mean(WAIST_HT_NATURAL)/12, ht_bott = mean(PATELLA.MID_HT)/12)

pfx_14 <- left_join(pfx_14, ansur_hts)

save(pfx_14, file = "~/Desktop/pfx_14.Rda")

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




