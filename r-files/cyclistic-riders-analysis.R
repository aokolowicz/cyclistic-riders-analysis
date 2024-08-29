# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)
library(rlang)
library(lubridate)
library(modeest)
library(ggplot2)

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
                    month = month(started_at)) %>%
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

# Ultimate data-cleaning
# Remove duplicates
tripdata <- tripdata %>% distinct(ride_id, .keep_all = TRUE)
# Remove trips which length < 0
tripdata <- tripdata %>% filter(!(ride_length < 0))
# How many rides longer than 1 day are per months?
tripdata %>% mutate(month = month(started_at)) %>% 
  filter(ride_length > days(1)) %>% group_by(month) %>%
  summarize(n())
# Remove trips which length > 24 hours
View(tripdata %>% filter((ride_length > days(1))) %>% 
  select(ride_length, end_station_id, end_station_name, end_lat) %>% 
  mutate(len = seconds_to_period(ride_length)) %>% 
  arrange((end_station_id)))
tripdata <- tripdata %>% filter(!(ride_length > days(1)))


# ANALYZE
# Calculate descriptive statistics for ride_length
tripdata %>% summarize(max_len = seconds_to_period(max(ride_length)),
                       min_len = seconds_to_period(min(ride_length)),
                       avg_len = seconds_to_period(mean(ride_length)),
                       median_len = seconds_to_period(median(ride_length)),
                       stdev_len = seconds_to_period(sd(ride_length)))
# Calculate descriptive statistics for ride_length per month
tripdata %>% mutate(month = month(started_at)) %>% 
  group_by(month) %>% summarize(rides = n(),
                                max_len = seconds_to_period(max(ride_length)),
                                min_len = seconds_to_period(min(ride_length)),
                                avg_len = seconds_to_period(mean(ride_length)),
                                median_len = seconds_to_period(median(ride_length)),
                                stdev_len = seconds_to_period(sd(ride_length)))
# Calculate the mode of day_of_week
mfv(tripdata$day_of_week)
# Calculate the mode of day_of_week per month per member_casual
tripdata %>% mutate(month = month(started_at)) %>% 
  group_by(member_casual, month) %>% summarize(mfv(tripdata$day_of_week)) %>% 
  print(n=24)
# Calculate the average ride_length for members and casual riders
tripdata %>% group_by(member_casual) %>%
  summarize(avg = mean(ride_length), median = median(ride_length))
# Calculate the average ride_length for members and casual riders per month
tripdata %>% mutate(month = month(started_at)) %>% 
  group_by(member_casual, month) %>%
  summarize(avg = mean(ride_length), median = median(ride_length)) %>% 
  print(n=24)
# Calculate the average ride_length for users by day_of_week
tripdata %>% group_by(member_casual, day_of_week) %>%
  summarize(avg = mean(ride_length), median = median(ride_length))
# Calculate the number of rides for users by day_of_week
tripdata %>% group_by(member_casual, day_of_week) %>%
  summarize(rides = n())
# The most popular start_station for members and casual riders
tripdata %>% group_by(member_casual, start_station_id) %>% 
  summarize(rides = n()) %>% arrange(desc(rides)) %>% print(n = 12)
# Determine start_station_name and coordinates
tripdata %>% mutate(lat = as.character(start_lat), lng = as.character(start_lng)) %>% 
  select(start_station_id, start_station_name, lat, lng) %>% 
  filter(start_station_id == "KA1504000135") %>% print(n = 1)
# Determine rideable_type per user
tripdata %>% group_by(member_casual, rideable_type) %>% 
  summarize(rides = n()) %>% arrange(desc(rides))


# DATA VIZ
# Stacked histogram of ride_length for members and casual riders (1 hr)
ggplot(tripdata, aes(x = ride_length, fill = member_casual)) +
  geom_histogram(binwidth = 120, boundary = 0, color = "black") +
  scale_fill_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  scale_x_continuous(limits = c(0, 3600),
                     breaks = seq(0, 3600, by = 600),
                     labels = scales::label_number(scale = 1 / 60)) + 
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(x = "Ride Length (min)",
       y = "Number of Rides",
       title = "Distribution of Ride Length",
       subtitle = "First Hour of Rental",
       caption = "Data from August 1, 2023 to July 31, 2024",
       fill = "")

# Prepare data by aggregating rides per day
trips_per_day <- tripdata %>%
  mutate(date = as.Date(started_at),
         mday = mday(started_at),
         month = month(started_at, label = TRUE, abbr = FALSE),
         year = year(started_at)) %>%
  group_by(date, mday, month, year, member_casual) %>%
  summarize(rides = n(), .groups = "drop")
# Plot with date as x-axis and faceting by month and year
ggplot(data = trips_per_day) +
  geom_line(aes(x = mday,
                y = rides,
                color = member_casual),
            linewidth = 1) +
  guides(color = guide_legend(reverse=TRUE)) +
  scale_color_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
  scale_y_continuous(limits = c(0, 2e4),
                     labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(x = "",
       y = "Number of Rides",
       title = "Daily Rides Over Time",
       subtitle = "Grouped by Membership Type",
       color = "") +
  facet_wrap(~ year + month, ncol = 3, scales = "free_x") + 
  theme(legend.position = "top")

# Prepare data by aggregating trips per user per month
trips_per_month <- tripdata %>% 
  mutate(year = year(started_at),
         month = month(started_at),
         month_name = month(started_at, label = TRUE, abbr = TRUE),
         month_year = factor(paste(month_name, "\n", year), 
                             levels = unique(paste(month_name, "\n", year)), 
                             ordered = TRUE)) %>% 
  group_by(member_casual, year, month, month_name, month_year) %>%
  summarize(avg = mean(ride_length), 
            median = median(ride_length),
            rides = n(),
            .groups = "drop")
# Plot combo chart for average ride_length and number of rides per month per user
ggplot(data = trips_per_month, aes(x = month_year)) +
  # Bar chart for number of rides
  geom_bar(aes(y = rides, 
               fill = member_casual), 
           stat = "identity", # Use actual values for the height
           position = "dodge", # Bars next to each other, not stacked
           alpha = 0.4) +
  # Line chart for average ride length per month
  geom_line(aes(y = avg * 333,  # Adjusted for scaling on the secondary axis
                color = member_casual, 
                group = member_casual),
            linewidth = 1) +
  # Custom color scales
  scale_color_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  scale_fill_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  # Primary y-axis for number of rides
  scale_y_continuous(name = "Number of Rides",
                     limits = c(0, 5e5),
                     labels = scales::label_number(scale = 1e-3, suffix = "k"),
                     # Secondary y-axis for average ride length
                     sec.axis = sec_axis(~ . / 333, # Adjust to match original scale (dot represents the original y-axis values)
                                         name = "Average Ride Length (min)",
                                         breaks = seq(0, 25 * 60, by = 5 * 60),
                                         labels = scales::label_number(scale = 1/60))) +
  labs(x = "",
       title = "Number of Rides and Average Ride Length by Month",
       subtitle = "Grouped by Membership Type",
       color = "Membership\nType",
       fill = "Membership\nType")

# Prepare data by aggregating trips per user per day_of_week
trips_per_dow <- tripdata %>%
  mutate(dow = wday(started_at, label = TRUE, abbr = FALSE)) %>% 
  group_by(member_casual, day_of_week, dow) %>%
  summarize(avg = mean(ride_length),
            median = median(ride_length),
            rides = n(),
            .groups = "drop")
# Plot combo chart for average ride_length and number of rides per day_of_week per user
ggplot(data = trips_per_dow, aes(x = dow)) +
  # Bar chart for number of rides
  geom_bar(aes(y = rides, 
               fill = member_casual), 
           stat = "identity", 
           position = "dodge", 
           alpha = 0.3) +
  # Line chart for average ride length per month
  geom_line(aes(y = avg * 500,  # Adjusted for scaling on the secondary axis
                color = member_casual, 
                group = member_casual),
            linewidth = 1) +
  # Custom color scales
  scale_color_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  scale_fill_manual(values = c("member" = "dodgerblue2", "casual" = "darkorange2")) +
  # Primary y-axis for number of rides
  scale_y_continuous(name = "Number of Rides",
                     limits = c(0, 7.5e5),
                     breaks = seq(0, 7.5e5, by=1.5e5),
                     labels = scales::label_number(scale = 1e-3, suffix = "k"),
                     # Secondary y-axis for average ride length
                     sec.axis = sec_axis(~ . / 500, # Adjust to match original scale (dot represents the original y-axis values)
                                         name = "Average Ride Length (min)",
                                         breaks = seq(0, 25 * 60, by = 5 * 60),
                                         labels = scales::label_number(scale = 1/60))) +
  labs(x = "",
       title = "Number of Rides and Average Ride Length by Day of Week",
       subtitle = "Grouped by Membership Type",
       caption = "Data from August 1, 2023 to July 31, 2024",
       color = "Membership\nType",
       fill = "Membership\nType")

# Determine rideable_type per user
trips_per_biketype <- tripdata %>%
  mutate(dow = wday(started_at, label = TRUE, abbr = TRUE)) %>%
  group_by(member_casual, day_of_week, dow, rideable_type) %>% 
  summarize(rides = n(), .groups = "drop")
# Bar plot of bike types popularity per user per day_of_week
ggplot(data = trips_per_biketype) +
  geom_bar(aes(x = dow,
               y = rides,
               fill = rideable_type),
           stat = "identity") +
  scale_fill_manual(values = c("classic_bike" = "pink4",
                               "docked_bike" = "grey20",
                               "electric_bike" = "cadetblue4")) +
  scale_y_continuous(limits = c(0, 7.5e5),
                     breaks = seq(0, 7.5e5, by=1.5e5),
                     labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  facet_wrap(~member_casual) +
  labs(x = "",
       y = "Number of Rides",
       title = "Number of Rides by Bike Type",
       subtitle = "Grouped by Membership Type",
       caption = "Data from August 1, 2023 to July 31, 2024",
       fill = "Bike Type")
