### Script 2 - just trip ####

library(tidyverse)
library(dplyr)

# Find the number of cancelled trips (<2min) and remove from data set 

nrow(trip_data$duration < 120)

trip_clean <- trip_data %>%
  filter(duration > 120)

# removed 2,585 trips

# Identify outliers in the data set. Record and remove from the data set.
# 1.5 *IQR  - Q1 OR 1.5*IQR + Q3

iqr <- IQR(trip_clean$duration)
# 404

Q <- quantile(trip_clean$duration, probs=c(.25, .75), na.rm = FALSE)


#up <-  Q[2]+1.5*iqr # Upper Range  
#low <- Q[1]-1.5*iqr # Lower Range

# https://www.r-bloggers.com/2020/01/how-to-remove-outliers-in-r/

# Establish the highest volume hours on weekdays. Use to build 'rush hours' into their model (lubridate package).
# Have to find the hours of weekdays where the trip volume is highest (eg. can try histograms). Just trip.


# Calculate the average utilization of bikes for each month. Total time used/total time in month.

