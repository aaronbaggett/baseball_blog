# ==========================================================================
# Home Plate Dimensions Including Baseball Radius
# ==========================================================================

### --- Load pacakges (if necessary) --- ###
library(ggplot2, quietly = TRUE, warn.conflicts = FALSE)

# Specify homeplate polygon coordinates
plate.x <- c(-0.70833, 0.70833, 0.70833, 0, -0.70833)
plate.y <- c(1.41666, 1.41666, 0.70833, 0, 0.70833)

# Draw homeplate polygon
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
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  theme(panel.border = element_blank())

