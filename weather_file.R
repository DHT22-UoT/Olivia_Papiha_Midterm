#Weather

#loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(stats)

#reading libraries 
weather_df <- read.csv("weather.csv")

#adding transformations 
weather_df_1 <- weather_df %>% 
  mutate(date = as.POSIXct(date, format="%d/%m/%y")) %>%
  mutate (precipitation_inches = str_replace(precipitation_inches, pattern="T", replacement="0")) %>% 
  mutate (precipitation_inches = as.numeric(precipitation_inches)) %>%
  mutate (cloud_cover = as.factor(cloud_cover)) %>%
  mutate (events = na_if(x=events, y="")) %>%
  mutate (events = as.factor(events)) %>%
  mutate (zip_code = as.factor(zip_code)) %>%
  mutate (city = as.factor(city))

#can still observe 1105 missing dates, will remove entirely
weather_df_2 <- weather_df_1 %>% filter(!is.na(date))

summary(weather_df_2)
#remove outliers
#exploratory data analysis showed outliers for
#columns: max_visibility_miles, mean_visibility_miles, max_wind_Speed_mph, max_gust_speed_mph 

#not removed for max_visibility_miles due to outer and lower bounds being the same 
quartile_max_visibility_miles <- quantile(weather_df_2$max_visibility_miles, probs=c(.25, .75), na.rm = T)
iqr_max_visibility_miles <- IQR(weather_df_2$max_visibility_miles, na.rm=T)
upper_lim_max_visibility_miles <- quartile_max_visibility_miles[2] + 1.5*iqr_max_visibility_miles
lower_lim_max_visibility_miles <- quartile_max_visibility_miles[1] - 1.5*iqr_max_visibility_miles 
  
#not removed for mean_visibility_miles due to outer and lower bounds being the same 
quartile_mean_visibility_miles <- quantile(weather_df_2$mean_visibility_miles, probs=c(.25, .75), na.rm = T)
iqr_mean_visibility_miles <- IQR(weather_df_2$mean_visibility_miles, na.rm=T)
upper_lim_mean_visibility_miles <- quartile_mean_visibility_miles[2] + 1.5*iqr_mean_visibility_miles
lower_lim_mean_visibility_miles <- quartile_mean_visibility_miles[1] - 1.5*iqr_mean_visibility_miles 
 
#removed for mean_visibility_miles due to outer and lower bounds being the same 
quartile_max_wind_speed_mph <- quantile(weather_df_2$max_wind_Speed_mph, probs=c(.25, .75), na.rm = T)
iqr_max_wind_Speed_mph <- IQR(weather_df_2$max_wind_Speed_mph, na.rm=T)
upper_lim_max_wind_Speed_mph <- quartile_max_wind_speed_mph[2] + 1.5*iqr_max_wind_Speed_mph
lower_lim_max_wind_Speed_mph <- quartile_max_wind_speed_mph[1] - 1.5*iqr_max_wind_Speed_mph 
#remove values or do you want to replace the outliers with upper-lim and lower-lim values?
weather_df_2 <- subset(weather_df_2, weather_df_2$max_wind_Speed_mph > (lower_lim_max_wind_Speed_mph) & weather_df_2$max_wind_Speed_mph < upper_lim_max_wind_Speed_mph)

#removed for mean_visibility_miles due to outer and lower bounds being the same 
quartile_max_gust_speed_mph <- quantile(weather_df_2$max_gust_speed_mph, probs=c(.25, .75), na.rm = T)
iqr_max_gust_speed_mph <- IQR(weather_df_2$max_gust_speed_mph, na.rm=T)
upper_max_gust_speed_mph <- quartile_max_gust_speed_mph[2] + 1.5*iqr_max_gust_speed_mph
lower_max_gust_speed_mph <- quartile_max_gust_speed_mph[1] - 1.5*iqr_max_gust_speed_mph 
weather_df_2 <- subset(weather_df_2, weather_df_2$max_gust_speed_mph > (lower_max_gust_speed_mph) & weather_df_2$max_gust_speed_mph < upper_max_gust_speed_mph)

summary(weather_df_2)
