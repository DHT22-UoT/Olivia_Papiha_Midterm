#Weather and Trip Correlation

library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)

trip_data <- read_csv("/Users/papihajoharapurkar/Downloads/babs\ 2/trip.csv")
weather_df <- read_csv("weather.csv")

#Data transformations for Trip Data 
# Find the number of cancelled trips (<2min) and remove from data set 
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

# trip_clean2 = 258645 obs, removed 65,195 obs 

# So date will be recognized as a date
trip_clean2 <- trip_clean2 %>% 
  mutate(start_date = as.POSIXct(start_date, format="%d/%m/%y")) %>%
  mutate(end_date = as.POSIXct(end_date, format="%d/%m/%y"))

head(trip_clean2)
# puts dates in recognizable formats 
for_start_date <- mdy_hm(trip_clean2$start_date)
for_end_date <- mdy_hm(trip_clean2$end_date)

mdy_hm(trip_clean2$start_date)

head(trip_clean2)

#Data Transformations for Weather Data 
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

table(trip_clean2$zip_code)
summary(weather_df_2)


#Joining station and trip to obtain trip-info
station_df <- read_csv("station.csv")
station_trip <- left_join(trip_clean2,station_df, by=c("start_station_name" = "name"))

#Joining weather and station_file based on start_date - only 1 observation for each UID pair= date and city 
joined_df <- left_join(station_trip,weather_df_2, by=c("start_date" = "date", "city"))
# unique(joined_df)
# dim(joined_df)
joined_df <- as.data.frame(joined_df)

attach(joined_df)
joined_df_1 <- joined_df %>% select-c(id.x, bike_id, subscription_ty, zip_code, id.y, lat, long, dock_count, installation_date)
head(joined_df_1)
corrplot(joined_df)

