# ==========================================================================
# Umpire Accuracy Dot Plot
# ==========================================================================

### --- Load pacakges (if necessary) --- ###
library(lme4, quietly = TRUE)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

### --- Read in multilevel data.frame *pfx_14* --- ###
load(url("http://aaronbaggett.com/data/pfx_14.rda"))

### --- Relevel "zone_reg" to make "ball" reference group --- ###
pfx_14$zone_reg <- relevel(pfx_14$zone_reg, ref = "ball")

### --- Relevel "bs_count" to make "neutral" reference group --- ###
pfx_14$bs_count <- relevel(pfx_14$bs_count, ref = "neutral")

### --- Find mean and sd calls made by umpires --- ###
pfx_14_sub <- pfx_14 %>%
  group_by(umpire) %>%
  #filter(length(u_test) >= 1000) %>%
  summarize(mean = mean(u_test), sd = sd(u_test))

### --- Subset data for dot plotting --- ###
by_umpire <- pfx_14 %>%
  group_by(umpire) %>%
  summarize(accuracy = mean(u_test), 
    se = sd(u_test) / sqrt(length(u_test)))

with(by_umpire, min(accuracy))
with(by_umpire, max(accuracy))
with(by_umpire, mean(accuracy))
with(by_umpire, sd(accuracy))

set.seed(1100)
id <- sample(1100:1175)

by_umpire$id <- id

# Sort *by_umpire* in descending order
sort_by_umpire <- by_umpire[order(-by_umpire$accuracy), ]

### --- Build dot plot --- ###
ggplot(data = sort_by_umpire, 
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
  scale_y_discrete("", labels = rev(sort_by_umpire$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())

pfx_accuracy <- pfx_14 %>%
  group_by(umpire) %>%
  summarize(accuracy = mean(u_test_pfx), 
    se = sd(u_test_pfx) / sqrt(length(u_test_pfx)))

with(pfx_accuracy, min(accuracy))
with(pfx_accuracy, max(accuracy))
with(pfx_accuracy, mean(accuracy))
with(pfx_accuracy, sd(accuracy))

with(pfx_accuracy, pfx_accuracy[order(accuracy),])
with(pfx_accuracy, pfx_accuracy[order(-accuracy),])

set.seed(1100)
id <- sample(1100:1175)

pfx_accuracy$id <- id

# Sort *pfx_accuracy* in descending order
sort_pfx_accuracy <- pfx_accuracy[order(-pfx_accuracy$accuracy), ]

### --- Build dot plot --- ###
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
    axis.text.y = element_text(size = 6)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())



pfx_accuracy <- pfx_14 %>%
  group_by(umpire) %>%
  summarize(accuracy = mean(u_test_pfx),
    se = sd(u_test_pfx) / sqrt(length(u_test_pfx)))

with(pfx_accuracy, min(accuracy))
with(pfx_accuracy, max(accuracy))
with(pfx_accuracy, mean(accuracy))
with(pfx_accuracy, sd(accuracy))

with(pfx_accuracy, pfx_accuracy[order(accuracy),])
with(pfx_accuracy, pfx_accuracy[order(-accuracy),])

set.seed(1100)
id <- sample(1100:1175)

pfx_accuracy$id <- id

# Sort *pfx_accuracy* in descending order
sort_pfx_accuracy <- pfx_accuracy[order(-pfx_accuracy$accuracy), ]

### --- Plate and zone width dot plot
# Plate Width
ggplot(data = sort_by_umpire, 
  aes(x = accuracy, y = sort(umpire, decreasing = TRUE))) +
  geom_vline(aes(xintercept = mean(accuracy)), 
    color = "dodgerblue", linetype = 2, size = 0.35) +
  geom_line(aes(group = 1), color = "dodgerblue") +
  geom_point(color = "dodgerblue") + 
# Zone Width
  geom_point(data = sort_pfx_accuracy, aes(x = accuracy, 
    y = sort(umpire, decreasing = TRUE)), color = "tomato") +
  geom_line(data = sort_pfx_accuracy, aes(group = 1), color = "tomato") +
  geom_vline(data = sort_pfx_accuracy, aes(xintercept = mean(accuracy)), color = "tomato", 
    linetype = 2, size = 0.35)+
  scale_x_continuous(limits = c(0.81, 0.955), breaks = seq(0.81, 0.955, 0.010), 
    name = "\nUmpire Decision Accuracy") +
  scale_y_discrete("", labels = rev(sort_by_umpire$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_blank())

### --- Build dot plot --- ###
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
  scale_x_continuous(limits = c(0.786, 0.947), breaks = seq(0.786, 0.947, 0.010), 
    name = "\nUmpire Decision Accuracy") +
  scale_y_discrete("Umpire ID\n", labels = rev(sort_pfx_accuracy$umpire)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10), 
    axis.text.x = element_text(size = 8)) +
  theme(axis.title.y = element_text(size = 10), 
    axis.text.y = element_text(size = 6)) +
  theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank())