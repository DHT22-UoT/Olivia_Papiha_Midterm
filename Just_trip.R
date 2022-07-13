### Script 2 - just trip ####

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

trip_data <- read_csv("trip.csv")

# Find the number of cancelled trips (<2min) and remove from data set 

nrow(trip_data$duration < 120) # returning null?

# Filter out rows more than 120s 
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

# trip_clean2 = 258,45 obs, removed 65,195 obs 

# Establish the highest volume hours on weekdays. Use to build 'rush hours' into their model (lubridate package).
# Have to find the hours of weekdays where the trip volume is highest (eg. can try histograms). Just trip.

# Convert format of start_date and end_date

# So date will be recognized as a date
# Attempted to mutate data and input format as "%d/%m/%y %H:%M" but dates are returning NA
as.POSIXct(trip_clean2$start_date)
as.POSIXct(trip_clean2$end_date)

glimpse(trip_clean2)
print(status(trip_clean2))


# Check if they are not being recognized correctly 
trip_clean2$for_start_date

rush_hours <- trip_clean2 %>%
  mutate(start_date_for = mdy_hm(start_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(start_date_for) >= 2 & wday(start_date_for) <= 6) %>%
  mutate(hrs = hour(mdy_hm(start_date))) %>%
  group_by(hrs)
 
# Est highest vol hours for each weekday with histogram
hist(rush_hours$hrs)

# And using a table
table(rush_hours$hrs)

# Determine the 10 most frequent starting stations and ending stations during the ‘rush hours’.

# Most frequent starting stations during rush hours on weekdays
rush_hours2 <- trip_clean2 %>%
  mutate(start_date_for = mdy_hm(start_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(start_date_for) >= 2 & wday(start_date_for) <= 6) %>%
  mutate(hrs = hour(mdy_hm(start_date))) %>%
  group_by(hrs) %>%
  group_by(start_station_name)

table(rush_hours2$start_station_name)

# Most frequent ending stations during rush hours on weekdays
rush_hours3 <- trip_clean2 %>%
  mutate(end_date_for = mdy_hm(end_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(end_date_for) >= 2 & wday(end_date_for) <= 6) %>%
  mutate(hrs = hour(mdy_hm(end_date))) %>%
  group_by(hrs) %>%
  group_by(end_station_name)

table(rush_hours3$end_station_name)
head((rush_hours3$end_station_name),10)

# Determine the 10 most frequent starting stations and ending stations during weekends. 

# Most frequent starting stations on weekends
rush_hours_wkend <- trip_clean2 %>%
  mutate(start_date_for = mdy_hm(start_date)) %>%
  # Removes everyday except for Sun (1) and Sat (7) from data
  filter(wday(start_date_for) == 1 | wday(start_date_for) == 7) %>%
  mutate(hrs = hour(mdy_hm(start_date))) %>%
  # Group by hours
  group_by(hrs) %>%
  # Group by start stations
  group_by(start_station_name)

table(rush_hours_wkend$start_station_name)

# Most frequent ending stations on weekends
rush_hours_wkend2 <- trip_clean2 %>%
  mutate(end_date_for = mdy_hm(end_date)) %>%
  # Removes everyday except for Sun (1) and Sat (7) from data
  filter(wday(end_date_for) == 1 | wday(end_date_for) == 7) %>%
  mutate(hrs = hour(mdy_hm(end_date))) %>%
  group_by(hrs) %>%
  group_by(end_station_name)

table(rush_hours_wkend2$end_station_name)


# Calculate the average utilization of bikes for each month. Total time used/total time in month.
# New df 
# Group by month (mutate)
# Sum duration per bike_id per month
# Add column for number of seconds in each month
# Add month summarise sum_duration/ no of sec per month

bike_utl <- trip_clean2 %>%
  mutate(start_date_for2 = mdy_hm(start_date)) %>%
  group_by(bike_id) %>%
  group_by(mon = month(start_date_for2))


bike_utl2 <- aggregate(bike_utl$duration, by=list(bike_id=bike_utl$bike_id ,mon=bike_utl$mon),FUN = sum)

bike_utl2 %>%
  # add column for number of days per month 
  mutate(days_in_month = ifelse(1, 31), 
                                ifelse(2, 28),
                                       ifelse(3, 31),
                                              ifelse(4, 30),
                                                     ifelse(5, 31),
                                                            ifelse(6, 30),
                                                                   ifelse(7, 31),
                                                                          ifelse(8, 31),
                                                                                 ifelse(9, 30),
                                                                                        ifelse(10,31),
                                                                                               ifelse(11,30),
                                                                                                      ifelse(12, 31)) %>%
# Add column with calculation seconds per month (86400 sec / day)
  mutate(sec_per_month = days_in_month*86400) %>%
  mutate(avg_utl_per_mon = x/sec_per_month)

# Summed the duration in seconds of trip starting in each month 
#bike_utl2 <- aggregate(bike_utl$duration, by=list(mon = bike_utl$month), FUN = sum)

#bike_utl2 %>%
# add column for number of days per month 
 #mutate(no_days_per_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) %>%
# Add column with calculation seconds per month (86400 sec / day)
#mutate(sec_per_month = no_days_per_month*86400)

