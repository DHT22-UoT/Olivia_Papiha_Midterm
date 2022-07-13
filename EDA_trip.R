### EDA Trip ###

library(funModeling)
library(tidyverse)
library(Hmisc)
library(plyr)

setwd("~/Users/olivia/Desktop/")
trip_data <- read_csv("trip.csv")

#### EDA for trip.data ####

# Setting up 

basic_eda <- function(trip_data)
{
  glimpse(trip_data)
  print(status(trip_data))
  freq(trip_data) 
  print(profiling_num(trip_data))
  plot_num(trip_data)
  describe(trip_data)
}

basic_eda(trip_data)

# Step 1: First approach to data

glimpse(trip_data)

status(trip_data)

# Noticed there are 70 unique station ids but 74 station names (both start and end stations)

# Looking at unique values in start station name to identify duplicates
unique(sort(trip_data$start_station_name))
# noticed "Post at Kearney" and "Post at Kearny" as well as "Washington at Kearney" and "Washington at Kearny"

# Looking at unique values in end station name to look for duplicates
unique(sort(trip_data$end_station_name))
# "Post at Kearney" and "Post at Kearny" and "Washington at Kearney" and "Washington at Kearney"

unique(sort(trip_data$start_station_id))

# Fixing misspelled station names 

trip_data$start_station_name[trip_data$start_station_name == "Post at Kearny"] <- "Post at Kearney"
trip_data$start_station_name[trip_data$start_station_name == "Washington at Kearny"] <- "Washington at Kearney"

trip_data$end_station_name[trip_data$end_station_name == "Post at Kearny"] <- "Post at Kearney"
trip_data$end_station_name[trip_data$end_station_name == "Washington at Kearny"] <- "Washington at Kearney"

start_stat_tab <- data_frame(unique(trip_data$start_station_name))
start_id_tab <- data_frame(unique(trip_data$start_station_id))

# Found "Broadway at Main" and "Stanford in Redwood City" are both coded under station ID 80
# Combining them into one station name 
trip_data$start_station_name[trip_data$start_station_name == "Broadway at Main"] <- "Stanford in Redwood City"
trip_data$end_station_name[trip_data$end_station_name == "Broadway at Main"] <- "Stanford in Redwood City"


# Found "San Jose Government Center" and "Santa Clara County Civic Center" are both coded under station ID 25
# Combining them into one station name 
trip_data$start_station_name[trip_data$start_station_name == "San Jose Government Center"] <- "Santa Clara County Civic Center"
trip_data$end_station_name[trip_data$end_station_name == "San Jose Government Center"] <- "Santa Clara County Civic Center"

status(trip_data)


library(lubridate)

# Changing dates from character to a date

#trip_data <- trip_data %>%
  #mutate(start_date = as.POSIXlt(start_date, format= "%d/%m/%y %H:%M")) %>%
  #mutate(end_date = as.POSIXlt(end_date, format= "%d/%m/%y %H:%M"))


# Step 2: Analyzing categorical variables - 

# Creating a plot of start station frequency 
freq(trip_data$start_station_name)
head(freq(trip_data$start_station_name), 10)

# Creating a plot of end station frequency 
freq(trip_data$end_station_name)
head(freq(trip_data$end_station_name), 10)

# Plot for subscription type
freq(trip_data$subscription_type)


# Step 3: Analyzing numerical variables - plot_num(trip_data)

# Runs for all numerical/integer variables automatically
profiling_num(trip_data$duration)
describe(trip_data$duration)

# Find the number of cancelled trips (<2min) and remove from data set 

# Filter rows more than 120s 
trip_clean <- trip_data %>%
  filter(duration >= 120)

# removed 2,499 trips

# Removing the outliers from duration

iqr_trip <- IQR(trip_clean$duration)
# 404

Q1 <- quantile(trip_clean$duration, .25)
Q3 <- quantile(trip_clean$duration, .75)

up <- 1.5*iqr_trip + Q3 # Upper Range  

low <- 1.5*iqr_trip - Q1 # Lower Range

# trip_clean = 323840 obs
trip_clean2 <- trip_clean

trip_clean2$duration[trip_clean2$duration > 1352.5] <- 1352.5
trip_clean2$duration[trip_clean2$duration < 259.5] <- 259.5

# trip_clean2 = 65,195 outliers were assigned as upper and lower limits instead of removing

# Plot histogram of the duration in seconds 
hist(trip_clean2$duration, main = "Histogram of Trip Duration", xlab = "Trip Duration in Seconds")

# Step 4: Analyzes numerical and categorical at the same time - 

describe(trip_clean2$duration)

# Check min and max values (outliers)
# Check Distributions (same as before)



