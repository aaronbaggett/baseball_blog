# ===========================================================
# R Code
# Exploring Baseball Data with R
# Post: Conceptualizing the MLB Strike Zone Using PITCHf/x Data Pt. 2
# 03/19/2015
# ===========================================================

# Load package libraries
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(lme4, quietly = TRUE)

# Read in 2014 PITCHf/x data
load(url("http://aaronbaggett.com/data/pfx_14.rda"))

carp <- pfx_14 %>% 
  #select(sz_top, sz_bot, player_sz_top, player_sz_bot) %>% 
  filter(batter_name == "Matt Carpenter")

shapiro.test(carp$sz_top)
shapiro.test(carp$sz_bot)

ggplot(data = carp, aes(x = sz_top)) + geom_density()
ggplot(data = carp, aes(x = sz_bot)) + geom_density()

heights <- read.csv("/Users/AB/Dropbox/Dissertation/Data/Anthropometric Data/Anthropometric_Measurements.csv")


ansur <- read.delim("~/baseball_blog/data/ansur_men.txt", header = TRUE)

ansur <- ansur %>% 
  select(STATURE, SUPRASTERNALE_HT, WAIST_HT_NATURAL, PATELLA.MID_HT)

ansur*0.0393701

STATURE: height
SUPRASTERNALE_HT: length from top of shoulders to ground
WAIST_HT.OMPHALION: length from top of pants to ground
WAIST_HT_NATURAL: length from midpoint between shoulders and waist and ground
OMPHALION: length between shoulders and waist
PATELLA.MID_HT: length from knee to ground

ansur$
