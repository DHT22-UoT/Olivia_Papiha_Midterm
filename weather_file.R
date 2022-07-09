#Weather

#loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)

#reading libraries 
weather_df <- read.csv("weather.csv")

#changing date to as.date column
weather_df1 <- weather_df %>% mutate(date = as.POSIXct(date, format = "%m/%d/%Y")) 
                       
                                     