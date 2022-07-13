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
  #changing events to character type 
  mutate (events = as.character(events)) %>%
  #recoding "" in events to NA 
  mutate (events = na_if(x=events, y="")) %>%
  #changing events to be coded as factor
  mutate (events = as.factor(events)) %>%
  #changing zipcode to be coded as factor
  mutate (zip_code = as.factor(as.character(zip_code))) %>%
  #changing city to be coded as factor 
  mutate (city = as.factor(city)) %>% 
  #changing max_visibility_miles to be coded as numeric 
  mutate (max_visibility_miles = as.numeric(max_visibility_miles)) %>% 
  #changing mean_visibility_miles to be coded as numeric 
  mutate (mean_visibility_miles = as.numeric(mean_visibility_miles)) %>% 
  #changing max_wind_Speed_mph to be coded as numeric 
  mutate (max_wind_Speed_mph = as.numeric(max_wind_Speed_mph)) %>% 
  #changing max_gust_speed_mph to be coded as numeric 
  mutate (max_gust_speed_mph = as.numeric(max_gust_speed_mph)) 
  
#analyzing categorical data 
freq(weather_df_1)

#analyzing numerical data 
plot_num(weather_df_1)

#full univariate analysis being performed 
profiling_num(weather_df_1)

#provides summary on both numerical and categorical data 
describe(weather_df_1)

#writing modified_weaher_data into weather_transform csv file
saveRDS(weather_df_1, "weather_transform.rds")

