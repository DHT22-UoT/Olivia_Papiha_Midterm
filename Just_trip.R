### Script 2 - just trip ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

trip_data <- read_csv("trip.csv")

# Find the number of cancelled trips (<2min) and remove from data set 

nrow(trip_data$duration < 120) # returning null?

trip_clean <- trip_data %>%
  filter(duration >= 120)

# removed 2,499 trips

# Identify outliers in the data set. Record and remove from the data set.
# 1.5 *IQR  - Q1 OR 1.5*IQR + Q3 to determine

iqr_trip <- IQR(trip_clean$duration)
# 404

Q1 <- quantile(trip_clean$duration, .25)
Q3 <- quantile(trip_clean$duration, .75)

up <- 1.5*iqr_trip + Q3 # Upper Range  

low <- 1.5*iqr_trip - Q1 # Lower Range

# trip_clean = 323840 obs
  
trip_clean2 <- subset(trip_clean, trip_clean$duration > (low) & trip_clean$duration < (up))
dim(trip_clean2)

# trip_clean2 = 258645 obs, removed 65,195 obs 

# Establish the highest volume hours on weekdays. Use to build 'rush hours' into their model (lubridate package).
# Have to find the hours of weekdays where the trip volume is highest (eg. can try histograms). Just trip.

# Convert format of start_date and end_date

# So date will be recognized as a date
as.POSIXct(trip_clean2$start_date)
as.POSIXct(trip_clean2$end_date)

# puts dates in recognizable formats 
for_start_date <- mdy_hm(trip_clean2$start_date)
for_end_date <- mdy_hm(trip_clean2$end_date)

mdy_hm(trip_clean2$start_date)

# establish which dates are weekdays and extract

wday(for_start_date, label = T)
hour(for_start_date)

rush_hours <- trip_clean2 %>%
  mutate(start_date_for = mdy_hm(start_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(start_date_for) >= 2 & wday(start_date_for) <= 6) %>%
  mutate(hours = hour(mdy_hm(start_date))) %>%
  group_by(hours) %>%
  tally()
 
# Est highest vol hours for each weekday with histogram
ggplot() + geom_histogram(aes(x=rush_hours$hours, y=rush_hours$n))

hist(rush_hours$hours)


# Calculate the average utilization of bikes for each month. Total time used/total time in month.
# New df 
# Group by month (mutate)
# Sum duration per month
# Add column for number of seconds in each month
# Add month summarise sum_duration/ no of sec per month

bike_utl <- trip_clean2 %>%
  mutate(start_date_for2 = mdy_hm(start_date)) %>%
  group_by(month = month(start_date_for2))

bike_utl2 <- aggregate(bike_utl$duration, by=list(month = bike_utl$month), FUN = sum)

bike_utl2 %>%
  # Add column with seconds per month 
  mutate(sec_per_month = c(2678400, 2419200, 2678400, 2592000,2678400,2592000,2678400,
                           2678400,2592000, 2678400,2592000,2678400)) %>%
  mutate(avg_utl_per_mon = x/sec_per_month)



