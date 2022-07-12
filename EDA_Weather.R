#Exploratory Data Analysis for Weather Data 

#Loading necessary libraries
library(tidyverse)
library(funModeling)
library(Hmisc)
library(dplyr)
library(lubridate)

#Uploading weather.dataset and assigning to weather_df object
weather_df <- read.csv("weather.csv")

#Overview on variables from each column and few observations
glimpse(weather_df)

#Profiling the data input 
status(weather_df)

#performing necessary transformations before visualization
weather_df_1 <- weather_df %>% 
  #changing date-column from character to as.date type 
  mutate(date = as.POSIXct(date, format="%d/%m/%y")) %>%
  #replacing T-values in precipitation column with 0 
  mutate (precipitation_inches = str_replace(precipitation_inches, pattern="T", replacement="0")) %>% 
  #changing precipitation from character to numeric  
  mutate (precipitation_inches = as.numeric(precipitation_inches)) %>%
  #changing cloud cover variable from character to factor  
  mutate (cloud_cover = as.factor(cloud_cover)) %>%
  #recoding "" in events to NA 
  mutate (events = na_if(x=events, y="")) %>%
  #changing events to be coded as factor
  mutate (events = as.factor(events)) %>%
  #changing zipcode to be coded as factor
  mutate (zip_code = as.factor(as.character(zip_code))) %>%
  #changing city to be coded as factor 
  mutate (city = as.factor(city))

#analyzing categorical data 
freq(weather_df_1)

#analyzing numerical data 
plot_num(weather_df_1)

#full univariate analysis being performed 
profiling_num(weather_df_1)

#provides summary on both numerical and categorical data 
describe(weather_df_1)
