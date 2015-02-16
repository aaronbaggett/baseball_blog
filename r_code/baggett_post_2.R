# ===========================================================
# R Code
# Exploring Baseball Data with R
# Post: Title
# xx/xx/2015
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