
# Read in libraries
library(tidyverse)
library(lubridate)
library(naniar)
library(visdat)

chicago_purpleair <- read_csv("chicago_historical_sensors.csv", col_names = TRUE)


# Review data
glimpse(chicago_purpleair)

# subset data
chicago_purpleair <- chicago_purpleair[,c(1:4,6,7,9)]

# Make new date column and find max and min
chicago_purpleair$date <- as_date(chicago_purpleair$time_stamp)
min_date <- min(chicago_purpleair$date)
max_date <- max(chicago_purpleair$date)

# Visualize missingness
gg_miss_var(chicago_purpleair, show_pct = TRUE) + 
  theme_bw() +
  ggtitle("Percent Missing of Each Variable")


# Distinct sensor_index?
distinct_sensor_index <- data[,1]%>% 
  distinct()

# Basic stats of pm2p5
data$avg_pm2p5 <- (data$pm2p5_cf_1_a + data$pm2p5_cf_1_b)/2

mean(data$avg_pm2p5, na.rm = TRUE)
median(data$avg_pm2p5, na.rm = TRUE)
min(data$avg_pm2p5, na.rm = TRUE)
max(data$avg_pm2p5, na.rm = TRUE)
sd(data$avg_pm2p5, na.rm = TRUE)


glimpse(data)
