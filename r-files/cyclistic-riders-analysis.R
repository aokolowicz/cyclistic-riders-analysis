# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
library(lubridate)
library(modeest)
library(ggplot2)

# Set working directory
setwd("C:/Users/Ola/Desktop/Adi/dev/google-data-analytics-certificate/cyclistic-riders-analysis")

# Import data from the subfolder
tripdata_202308 <- read_csv("raw-data/202308-divvy-tripdata.csv")
tripdata_202309 <- read_csv("raw-data/202309-divvy-tripdata.csv")
tripdata_202310 <- read_csv("raw-data/202310-divvy-tripdata.csv")
tripdata_202311 <- read_csv("raw-data/202311-divvy-tripdata.csv")
tripdata_202312 <- read_csv("raw-data/202312-divvy-tripdata.csv")
tripdata_202401 <- read_csv("raw-data/202401-divvy-tripdata.csv")
tripdata_202402 <- read_csv("raw-data/202402-divvy-tripdata.csv")
tripdata_202403 <- read_csv("raw-data/202403-divvy-tripdata.csv")
tripdata_202404 <- read_csv("raw-data/202404-divvy-tripdata.csv")
tripdata_202405 <- read_csv("raw-data/202405-divvy-tripdata.csv")
tripdata_202406 <- read_csv("raw-data/202406-divvy-tripdata.csv")
tripdata_202407 <- read_csv("raw-data/202407-divvy-tripdata.csv")

# Merge rows from all dataframes into one dataframe
tripdata <- rbind(tripdata_202308, tripdata_202309, tripdata_202310, tripdata_202311,
                  tripdata_202312, tripdata_202401, tripdata_202402, tripdata_202403,
                  tripdata_202404, tripdata_202405, tripdata_202406, tripdata_202407)

# OR
tripdata <- read_csv("tripdata-merged.csv")

# Explore the data
head(tripdata)
str(tripdata)
glimpse(tripdata)
colnames(tripdata)

# Add ride_length column
tripdata <- tripdata %>% mutate(ride_length = ended_at - started_at)
head(tripdata)

# Determine the day of the week the trip started (0 is Sunday, 6 is Saturday)
# and change the numbers to 1 is Sunday, 7 is Saturday
tripdata <- tripdata %>% mutate(day_of_week = as.POSIXlt(started_at)$wday + 1)

# Ensure column names consistency
tripdata <- clean_names(tripdata)
head(tripdata)

# Check for inconsistencies
# Count the numbers of members & casual riders
colnames(tripdata)
tripdata %>% group_by(member_casual) %>% tally()
# Count the numbers of bike types
tripdata %>% group_by(rideable_type) %>% tally()
# Check if trip not started or not ended in the analyzed period
tripdata %>% filter(started_at <= "2023-07-31")
tripdata %>% filter(ended_at <= "2023-07-31")
tripdata %>% filter(started_at >= "2024-08-01")
tripdata %>% filter(ended_at >= "2024-08-01")
# Check max and min ride_length
tripdata %>% summarize(max_len_h = seconds_to_period(max(ride_length)),
                       min_len_h = seconds_to_period(min(ride_length)))
# Number of trips ended before the rent (sic!)
tripdata %>% filter(ride_length < 0) %>% group_by(member_casual) %>% 
  tally()
# Number of trips longer than one day
tripdata %>% filter(ride_length > days(1)) %>% group_by(member_casual) %>%
  tally()

# Check for truncated data
View(tripdata %>% mutate(date = as.Date(started_at)) %>%
  group_by(date) %>% tally())
# Check the number of days in each month of trips
tripdata %>% mutate(date = as.Date(started_at),
                    month = as.POSIXlt(started_at)$mon + 1) %>%
  group_by(date, month) %>% summarize() %>% 
  group_by(month) %>% summarize(no_of_days = n())

# Check for duplicates
View(tripdata %>% group_by(ride_id) %>% summarize(n = n()) %>%
  arrange(desc(n)) %>% 
  filter(n > 1))

# Check if there are NA values
# sym(col) converts the column name (a string) into a symbol
# !! unquotes the symbol, allowing dplyr to evaluate it within filter()
for (col in colnames(tripdata)) {
  print(col)
  print(tripdata %>% filter(is.na(!!sym(col))))
}

# Check if missing station ids and names can be filled in
# Non-missing start_station_name values
View(tripdata %>% filter(!is.na(start_station_name)) %>% 
  select(start_station_name, start_station_id, start_lat, start_lng))
# Missing start_station_name values
View(tripdata %>% filter(is.na(start_station_name)) %>% 
  select(start_station_name, start_station_id, start_lat, start_lng))
# Non-missing end_station_name values and non-missing end_lat and end_lng values
View(tripdata %>% filter(!(is.na(end_station_name)&is.na(end_lat)&is.na(end_lng))) %>% 
  select(end_station_name, end_station_id, end_lat, end_lng))
# Missing end_station_name values and non-missing end_lat and end_lng values
View(tripdata %>% filter(is.na(end_station_name)&!(is.na(end_lat)&is.na(end_lng))) %>% 
  select(end_station_name, end_station_id, end_lat, end_lng))
# Missing end_station_name, end_lat, and end_lng values
View(tripdata %>% filter(is.na(end_station_name)&is.na(end_lat)&is.na(end_lng)) %>% 
  select(end_station_name, end_station_id, end_lat, end_lng))

# Ultimate data-cleaninig
# Remove duplicates
tripdata <- tripdata %>% distinct(ride_id, .keep_all = TRUE)
# Remove trips which length < 0
tripdata <- tripdata %>% filter(!(ride_length < 0))



# ANALYZE
# Calculate descriptive statistics for ride_length
tripdata %>% summarize(max_len = seconds_to_period(max(ride_length)),
                       min_len = seconds_to_period(min(ride_length)),
                       avg_len = seconds_to_period(mean(ride_length)),
                       median_len = seconds_to_period(median(ride_length)),
                       stdev_len = seconds_to_period(sd(ride_length)))
# Calculate the mode of day_of_week
mfv(tripdata$day_of_week)
# Calculate the average ride_length for members and casual riders
tripdata %>% group_by(member_casual) %>% summarize(avg = mean(ride_length),
                                                   median = median(ride_length))
# Calculate the average ride_length for users by day_of_week
tripdata %>% group_by(member_casual, day_of_week) %>%
  summarize(avg = mean(ride_length),
            median = median(ride_length))
# Calculate the number of rides for users by day_of_week
tripdata %>% group_by(member_casual, day_of_week) %>%
  summarize(rides = n())

tripdata <- tripdata %>% filter(ride_length > 100000)
hist(as.numeric(tripdata$ride_length))
