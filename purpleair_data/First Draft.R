# Read in libraries
library(tidyverse)
library(lubridate)
library(naniar)
library(visdat)

library(dplyr)
library(lubridate)


df=aug_w1_23
View(df)

# Make new date column and find max and min
df$date <- as_date(df$time_stamp)
min_date <- min(df$date)
max_date <- max(df$date)

# Visualize missingness
gg_miss_var(df, show_pct = TRUE) + 
  theme_bw() +
  ggtitle("Percent Missing of Each Variable")



# Basic stats of pm2p5
df$avg_pm2p5 <- (df$pm2p5_cf_1_a + df$pm2p5_cf_1_b)/2

mean(df$avg_pm2p5, na.rm = TRUE)
median(df$avg_pm2p5, na.rm = TRUE)
min(df$avg_pm2p5, na.rm = TRUE)
max(df$avg_pm2p5, na.rm = TRUE)
sd(df$avg_pm2p5, na.rm = TRUE)



########## 

# Convert time_stamp to POSIXct if it's not already
df$time_stamp <- as.POSIXct(df$time_stamp)

# Filter data starting from the specified timestamp
df_filtered <- df %>%
  filter(time_stamp >= as.POSIXct("2023-08-01 00:00:01"))

# Round time_stamp to the nearest 5 minutes
df_filtered <- df_filtered %>%
  mutate(time_stamp = round_date(time_stamp, "5 mins"))


# Group by rounded time_stamp and select the first row in each group
df_subset <- df_filtered %>%
  group_by(time_stamp) %>%
  slice(1)


View(df_subset)
print(df_subset)






# Make new date column and find max and min
df_subset$date <- as_date(df_subset$time_stamp)
min_date <- min(df_subset$date)
max_date <- max(df_subset$date)



# Basic stats of pm2p5
df_subset$avg_pm2p5 <- (df_subset$pm2p5_cf_1_a + df_subset$pm2p5_cf_1_b)/2

mean(df_subset$avg_pm2p5, na.rm = TRUE)
median(df_subset$avg_pm2p5, na.rm = TRUE)
min(df_subset$avg_pm2p5, na.rm = TRUE)
max(df_subset$avg_pm2p5, na.rm = TRUE)
sd(df_subset$avg_pm2p5, na.rm = TRUE)










# Count missing values in each column
missing_pm2p5_a <- sum(is.na(df_subset$pm2p5_cf_1_a))
missing_pm2p5_b <- sum(is.na(df_subset$pm2p5_cf_1_b))

missing_humidity <- sum(is.na(df_subset$humidity))





# Calculate total number of rows
total_rows <- nrow(df_subset)

# Calculate percent missing for each column
percent_missing_pm2p5_a <- (missing_pm2p5_a / total_rows) * 100
percent_missing_pm2p5_b <- (missing_pm2p5_b / total_rows) * 100

percent_missing_humidity <- (missing_humidity / total_rows) * 100

# avg humidity
average_humidity <- mean(df_subset$humidity, na.rm = TRUE)





####EPA

corrected_PM25 <- function(avg_pm2p5, humidity) {
  PM25 <- 0.52 * avg_pm2p5 - 0.085 * humidity + 5.71
  return(PM25)
}




df_subset2 <- df_subset %>%
  mutate(PM25 = corrected_PM25(avg_pm2p5, humidity))

View(df_subset2)








###MDPI

adjusted_PM25 <- function(avg_pm2p5, humidity) {
  if (avg_pm2p5 < 570) {
    PM25 <- avg_pm2p5 * 0.524 - 0.0862 * humidity + 5.75
  } else if (570 <= avg_pm2p5 && avg_pm2p5 < 611) {
    PM25 <- (0.0244 * avg_pm2p5 - 13.9) * (avg_pm2p5^2 * 4.21 * 10^(-4) + avg_pm2p5 * 0.392 + 3.44) + (1 - (0.0244 * pm2p5_cf_1_a - 13.9)) * (pm2p5_cf_1_a * 0.524 - 0.0862 * humidity + 5.75)
  } else {
    PM25 <- avg_pm2p5^2 * 4.21 * 10^(-4) + avg_pm2p5 * 0.392 + 3.44
  }
  return(PM25)
}

df_subset3 <- df_subset %>% 
  filter(!is.na(avg_pm2p5) & !is.na(humidity)) %>%
  mutate(PM25 = adjusted_PM25(avg_pm2p5, humidity))

View(df_subset3)







