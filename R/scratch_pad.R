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

