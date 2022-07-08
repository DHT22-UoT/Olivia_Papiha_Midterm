### EDA Trip ###

library(funModeling)
library(tidyverse)
library(Hmisc)

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

# Step 1: First approach to data - glimpse(trip_data)

status(trip_data)

# Step 2: Analyzing categorical variables - freq(trip_data)

hist(trip_data$)

# Step 3: Analyzing numerical variables - plot_num(trip_data)

# freq(data, path_out = ".") to export to pdf

# Runs for all numerical/integer variables automatically
data_prof=profiling_num(trip_data)

# Step 4: Analyzes numerical and categorical at the same time - describe(trip_data)

# Check min and max values (outliers)
# Check Distributions (same as before)



