# Load libraries
library(tidyverse)
library(dplyr)
library(readr)
library(janitor)

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
tripdata %>% group_by(rideable_type) %>% tally()
# Check if there are NA values in station names
col <- start_station_name
tripdata %>% filter(is.na(col))

for (col in colnames(tripdata)) {
  print(col)
  print(head(tripdata %>% filter(is.na(tripdata[col]))))
}

tripdata %>% group_by(end_station_name) %>% tally() %>% arrange(-n)
