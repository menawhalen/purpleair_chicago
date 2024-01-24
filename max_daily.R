# Read in libraries
library(tidyverse)
library(lubridate)

# read in purple air
chicago_purpleair <- read_csv("Data/chicago_historical_sensors copy.csv", col_names = TRUE)
chicago_sensor_ids <- read_csv("Data/chicago_sensor_ids.csv", col_names = TRUE)

# Review data
glimpse(chicago_purpleair)

# subset data to date and sensor
chicago_purpleair <- chicago_purpleair[,c(1,2)]


# subset to only 2022 and only keep timestamps that are after date created
chicago_purpleair <- chicago_purpleair %>%
  #filter(year(time_stamp) == 2022) %>%
  left_join(chicago_sensor_ids, by = "sensor_index") %>%
  select(sensor_index, date_created, time_stamp) %>%
  filter(time_stamp >= date_created)


# # find time inbetween
# chicago_purpleair <- chicago_purpleair %>%
#   mutate(time_stamp = as.POSIXct(time_stamp, origin="1970-01-01")) %>%
#   group_by(sensor_index) %>%
#   arrange(sensor_index, time_stamp) %>%
#   mutate(time_diff = as.numeric(difftime(time_stamp, lag(time_stamp), units = "secs"))) %>%
#   ungroup()


# group by sensor and date and count number of rows
daily_sensor_counts <- chicago_purpleair %>%
  mutate(date = as.Date(time_stamp)) %>%
  group_by(sensor_index, date) %>%
  summarise(count = n())


# make data frame ith all days in 2022
start_date <- as.Date("2022-01-01")
end_date <- as.Date("2022-12-31")
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
dates_df <- data.frame(Date = date_sequence)

# make data frame with all sensor index/days combos that should exist
sensors_2022 <- unique(chicago_purpleair$sensor_index)
sensor_index_vec <- rep(sensors_2022, each = 365)
day_year_vec <- rep(dates_df$Date, length(unique(chicago_purpleair$sensor_index)))

day_sensor_2022 <- data.frame(sensor_index_vec, day_year_vec)
names(day_sensor_2022) <- c("sensor_index", "date")

# merge this with daily sensor counts
obs_day_df <- day_sensor_2022 %>%
  left_join(daily_sensor_counts, by = c("sensor_index", "date")) %>%
  mutate(count = coalesce(count, 0))


##############################################################################
# new max attempt

# add date, order, group
max_df <- chicago_purpleair %>%
  mutate(date = as_date(time_stamp, tz = "UTC")) %>%
  group_by(sensor_index, date) %>%
  arrange(sensor_index, date, time_stamp) 

# create column of time difference between start of day and first observation
max_df <- max_df %>%
  mutate(time_diff_start = difftime(time_stamp[1], 
         as_datetime(paste(date[1], "00:00:00"), tz = "UTC"), 
         units = "secs"))

# negative values check time zone
attr(max_df$date, "tzone") # time_stamp is UTC
glimpse(max_df)

# create column of time difference between all observations in day
max_df <- max_df %>%
  mutate(time_diff = c(NA, as.numeric(diff(time_stamp), units = "secs")))

# create column of time difference between end of day and last observation
max_df <- max_df %>%
  mutate(time_to_end_of_day = as.numeric(difftime(
    as_datetime(paste(date[n()], "23:59:59"), tz = "UTC"), 
    time_stamp[n()], units = "secs"))) 

max_intervals <- max_df %>%
  summarise(max_interval = 
              as.numeric(max(c(time_diff_start,time_diff, time_to_end_of_day[1]), na.rm = TRUE)))


# merge max df with obs day
obs_max_df <- left_join(obs_day_df, max_intervals, by = c("sensor_index", "date"))

# write file to computer
#write.csv(obs_max_df, "daily_observations_count.csv")



    