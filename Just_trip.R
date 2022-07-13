### Script 2 - just trip ####

library(tidyverse)
library(dplyr)
library(lubridate)

trip_clean2<- readRDS("trip_clean2.rds")

# Establish the highest volume hours on weekdays. Use to build 'rush hours' into their model (lubridate package).
# Have to find the hours of weekdays where the trip volume is highest (eg. can try histograms). Just trip.

# Convert format of start_date and end_date

# So date will be recognized as a date
# Attempted to mutate data and input format as "%d/%m/%y %H:%M" but dates are returning NA
as.POSIXct(trip_clean2$start_date)
as.POSIXct(trip_clean2$end_date)

rush_hours <- trip_clean2 %>%
  mutate(start_date_for = mdy_hm(start_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(start_date_for) >= 2 & wday(start_date_for) <= 6) %>%
  mutate(hrs = hour(mdy_hm(start_date))) %>%
  group_by(hrs)
 
# Est highest vol hours for each weekday with histogram
hist(rush_hours$hrs, main = "Histogram of Rush Hours During Weekdays", xlab = "Hour of Day", xlim = c(0,24))

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
  filter(hrs == 8| hrs == 17 | hrs == 9) %>%
  group_by(start_station_name)

rush_hours2_tb <- table(rush_hours2$start_station_name)

rush_hours2_tb[order(rush_hours2_tb,decreasing = TRUE)]

# Most frequent ending stations during rush hours on weekdays
# Determine rush hours for ending stations
rush_hours3 <- trip_clean2 %>%
  mutate(end_date_for = mdy_hm(end_date)) %>%
  # Removes Sun (1) and Sat (7) from data
  filter(wday(end_date_for) >= 2 & wday(end_date_for) <= 6) %>%
  mutate(hrs = hour(mdy_hm(end_date))) %>%
  group_by(hrs) %>% 
  group_by(end_station_name)

table(rush_hours3$hrs)

rush_hours3 %>%
  # Filtering for identified rush hours
  filter(hrs == 8| hrs == 17 | hrs == 9)
  
rush_hours3_tb <- table(rush_hours3$end_station_name)

rush_hours3_tb[order(rush_hours3_tb,decreasing = TRUE)]

#### Determine the 10 most frequent starting stations and ending stations during weekends. ####

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

rush_hours_wkend_tb <- table(rush_hours_wkend$start_station_name)
rush_hours_wkend_tb[order(rush_hours_wkend_tb,decreasing = TRUE)]

# Most frequent ending stations on weekends
rush_hours_wkend2 <- trip_clean2 %>%
  mutate(end_date_for = mdy_hm(end_date)) %>%
  # Removes everyday except for Sun (1) and Sat (7) from data
  filter(wday(end_date_for) == 1 | wday(end_date_for) == 7) %>%
  mutate(hrs = hour(mdy_hm(end_date))) %>%
  group_by(hrs) %>%
  group_by(end_station_name)

rush_hours_wkend2_tb <- table(rush_hours_wkend2$end_station_name)
rush_hours_wkend2_tb[order(rush_hours_wkend2_tb,decreasing = TRUE)]


#### Calculate the average utilization of bikes for each month. Total time used/total time in month. ####

# Calculating average utilization of bikes total per month

bike_utl <- trip_clean2 %>%
  mutate(start_date_for2 = mdy_hm(start_date)) %>%
  group_by(mon = month(start_date_for2))
     
# Summed the duration in seconds of trip starting in each month 
bike_utl2 <- aggregate(bike_utl$duration, by=list(mon = bike_utl$mon), FUN = sum)

bike_utl2 %>%
  # Add column for number of days per month 
 mutate(no_days_per_month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) %>%
  # Add column with calculation seconds per month (86400 sec / day)
  mutate(sec_per_month = no_days_per_month*86400) %>%
  mutate(avg_utl_per_mon = x/sec_per_month)
  