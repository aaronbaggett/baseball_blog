### ----------------------------------------------------------------------------- ###
### --------------------- PITCHf/x Database Construction ------------------------ ###
### ----------------------------------------------------------------------------- ###

### --- Load package libraries (if necessary) --- ###
library(dplyr, warn.conflicts = FALSE)
library(pitchRx)

### --- Set and create *pfx_14a* working directory --- ###
#setwd("~/Dropbox/Umpire IRT Study/Data")

### --- Initialize SQLite database --- ###
#pfx_14 <- src_sqlite("pfx_14.sqlite3", create = TRUE)

### --- Set XML files to collect --- ###
#files <- c("inning/inning_all.xml", "players.xml", "miniscoreboard.xml")

### --- Scrape 2011 PITCHf/x data --- ###
#scrape(start = "2013-01-01", end = "2013-12-31", suffix = files, connect = pfx_14$con)

### --- Load SQLite table --- ###
pfx_14_all <- src_sqlite("~/Dropbox/pfx_data/pfx_14.sqlite3")

### --- Convert *pfx_14_all* tables to standalone tables --- ###
# action <- tbl(pfx_14_all, "action")
atbat <- tbl(pfx_14_all, "atbat")
# coach <- tbl(pfx_14_all, "coach")
# game <- tbl(pfx_14_all, "game")
# media <- tbl(pfx_14_all, "media")
pitch <- tbl(pfx_14_all, "pitch")
# player <- tbl(pfx_14_all, "player")
# po <- tbl(pfx_14_all, "po")
# runner <- tbl(pfx_14_all, "runner")
umpire <- tbl(pfx_14_all, "umpire")

### --- Chaining operations for data munging (I hate that word tho!) --- ###
atbats <- atbat %>%
  select(num, stand, b_height, batter_name, gameday_link) %>%
  group_by(gameday_link)
atbats <- collect(atbats)

pitches <- pitch %>%
  select(call = des, sz_top, sz_bot, px, pz, zone, num, count, gameday_link) %>%
  filter(call == "Called Strike" | call == "Ball") %>%
  group_by(gameday_link)
pitches <- collect(pitches)

umpires <- umpire %>%
  select(umpire = name, gameday_link) %>%
  filter(position == home) %>%
  group_by(gameday_link)
umpires <- collect(umpires)

### --- Join *atbat*, *pitch*, and *umpire* by *gameday_link* --- ###
ps_abs <- left_join(pitches, atbats, by = c("num", "gameday_link"))
ps_abs_us <- left_join(ps_abs, umpires, by = "gameday_link", copy = TRUE)

pfx_14 <- tbl_df(as.data.frame(ps_abs_us, n = -1))

### --- Exclude missing pitches --- ###
pfx_14 <- na.omit(pfx_14)

pfx_14 <- unique(pfx_14)

### --- Drop rows 1:8 with no *b_height* --- ###
# pfx_14 <- pfx_14[-1:-8, ]

### --- Write out *pfx_14* to CSV --- ###
# write.csv(pfx_14, "~/Desktop/pfx_14.csv")

### --- Read in *pfx_14* --- ###
# pfx_14 <- fread("~/Desktop/pfx_14.csv")

### --- Add player sz limits by *batter_name* and *b_height* --- ###
pfx_14 <- pfx_14 %>%
  group_by(batter_name) %>%
  mutate(player_sz_top = mean(sz_top), player_sz_bot = mean(sz_bot))

pfx_14 <- pfx_14 %>%
  group_by(b_height) %>%
  mutate(height_top = mean(sz_top), height_bot = mean(sz_bot))

### --- Create *u_test* variable for umpire's decision [1 = correct] --- ###
# Ball radius = ((1.57*2 + 17) / 12) / 2
pfx_14$u_test <- with(pfx_14,
    ifelse(call == "Ball" & px < -0.8110375 | px > 0.8110375 | 
      pz < player_sz_bot | pz > player_sz_top, 1,
    ifelse(call == "Called Strike" & pz > player_sz_bot & pz < player_sz_top & 
      px >= -0.8110375 & px <= 0.8110375, 1,
    ifelse(call == "Ball" & pz > player_sz_bot & pz < player_sz_top & 
      px > -0.8110375 & px < 0.8110375, 0,
    ifelse(call == "Called Strike" & px < -0.8110375 | px > 0.8110375 | 
      pz < player_sz_bot | pz > player_sz_top, 0, 99)))))

with(pfx_14, mean(as.numeric(u_test)))
table(pfx_14$u_test)

### --- Specify *count* advantages --- #
# 1. Add *bs_count* variable to indicate who has the advantage (p vs. b)
# Based on Marchi & Albert, 2014
pfx_14$bs_count <- with(pfx_14,

  # 1.1 Neutral
  ifelse(count == "0-0" | count == "1-0" | 
         count == "1-1" | count == "2-1", "neutral",

  # 1.2 Batter
  ifelse(count == "2-0" | count == "3-0" | 
  	 count == "3-1" | count == "3-2", "batter",

  # 1.3 Pitcher
  ifelse(count == "0-1" | count == "0-2" | 
         count == "1-2" | count == "2-2", "pitcher", 99)
)))

# 2. Convert *count* to factor
pfx_14$bs_count <- as.factor(pfx_14$bs_count)

### --- Respecify *zone* regions --- #
pfx_14$zone_reg <- with(pfx_14,

  # 1. RHBs
  ifelse(stand == "R" & zone == "1" | 
		 stand == "R" & zone == "4" | 
         stand == "R" & zone == "7", "inner",

  ifelse(stand == "R" & zone == "2" | 
         stand == "R" & zone == "5" | 
         stand == "R" & zone == "8", "middle",

  ifelse(stand == "R" & zone == "3" | 
         stand == "R" & zone == "6" | 
         stand == "R" & zone == "9", "outer",

  # 2. LHBs
  ifelse(stand == "L" & zone == "1" | 
         stand == "L" & zone == "4" | 
         stand == "L" & zone == "7", "outer",

  ifelse(stand == "L" & zone == "2" | 
         stand == "L" & zone == "5" | 
         stand == "L" & zone == "8", "middle",

  ifelse(stand == "L" & zone == "3" | 
         stand == "L" & zone == "6" | 
         stand == "L" & zone == "9", "inner", "ball")))))))

# 3. Convert *zone* to factor
pfx_14$zone_reg <- as.factor(pfx_14$zone_reg)

### --- Relevel "zone_reg" to make "ball" reference group --- ###
pfx_14$zone_reg <- relevel(pfx_14 $zone_reg, ref = "ball")

### --- Relevel "bs_count" to make "neutral" reference group --- ###
pfx_14 $bs_count <- relevel(pfx_14 $bs_count, ref = "neutral")

pfx_14$h_dist <- with(pfx_14, ifelse(px < 0, px - (-0.87),
  ifelse(px > 0, 0.87 - px, 99)))

pfx_14$h_mid <- (pfx_14$player_sz_top - pfx_14$player_sz_bot) / 2
pfx_14$h_mid <- pfx_14$h_mid + pfx_14$player_sz_bot

pfx_14$v_dist <- with(pfx_14, ifelse(pz < h_mid, player_sz_bot - pz, 
  ifelse(pz > h_mid, player_sz_top - pz, 99)))

### Read in 2013 umpire-level ###
u_14 <- read.csv("~/Dropbox/Umpire IRT Study/Data/2014_umpires.csv")
u_14$umpire <- as.character(u_14$umpire)

pfx_14 <- inner_join(pfx_14, u_14, by = "umpire")

### --- Rearrange variables --- ###
pfx_14 <- pfx_14 %>%
  select(c(gameday_link, batter_name, b_height, stand, sz_top, sz_bot, 
  player_sz_top, player_sz_bot, height_top, height_bot, px, pz, 
  count, bs_count, h_dist, h_mid, v_dist, zone, zone_reg, 
  umpire, yr_exp, call, u_test))

save(pfx_14, file = "~/Desktop/pfx_14.rda")
