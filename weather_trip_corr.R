#Weather and Trip Correlation Plot

#Loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(corrplot)

#reading in all relevant data files 
trip_data <- read_csv("/Users/papihajoharapurkar/Downloads/babs\ 2/trip.csv")
station_df <- read_csv("station.csv")
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
# for_start_date <- mdy_hm(trip_clean2$start_date)
# for_end_date <- mdy_hm(trip_clean2$end_date)

# mdy_hm(trip_clean2$start_date)

#Data Transformations for Weather Data 
#adding transformations to variables as seen from previous-weather-analysis 
weather_df_1 <- weather_df %>% 
  mutate(date = as.POSIXct(date, format="%d/%m/%y")) %>%
  mutate (precipitation_inches = str_replace(precipitation_inches, pattern="T", replacement="0")) %>% 
  mutate (precipitation_inches = as.numeric(precipitation_inches)) %>%
  mutate (cloud_cover = as.factor(cloud_cover)) %>%
  mutate (events = na_if(x=events, y="")) %>%
  mutate (events = as.factor(events)) %>%
  mutate (zip_code = as.factor(zip_code)) %>%
  mutate (city = as.factor(city)) %>% 
  mutate (max_wind_Speed_mph = as.numeric(max_wind_Speed_mph)) %>%
  mutate (max_gust_speed_mph = as.numeric(max_gust_speed_mph)) %>%
  mutate (max_visibility_miles = as.numeric(max_visibility_miles)) %>%
  mutate (mean_visibility_miles = as.numeric(mean_visibility_miles))

#removing NA values from weather_df_1, assigning to weather_df_2
weather_df_2 <- weather_df_1 %>% filter(!is.na(date))

#outlier removal attempt for max_visibility_miles, indicates that both upper and lower limit are the same 
quartile_max_visibility_miles <- quantile(weather_df_2$max_visibility_miles, probs=c(.25, .75), na.rm = T)
iqr_max_visibility_miles <- IQR(weather_df_2$max_visibility_miles, na.rm=T)
upper_lim_max_visibility_miles <- quartile_max_visibility_miles[2] + 1.5*iqr_max_visibility_miles
lower_lim_max_visibility_miles <- quartile_max_visibility_miles[1] - 1.5*iqr_max_visibility_miles 
#outlier removal based on percentiles:
upper_lim_max_visibility_miles <- quantile(weather_df_2$max_visibility_miles, 0.975, na.rm=T)
lower_lim_max_visibility_miles <- quantile(weather_df_2$max_visibility_miles, 0.025, na.rm=T)
weather_df_2 <- weather_df_2 %>% 
  mutate(max_visibility_miles = case_when(max_visibility_miles > as.numeric(upper_lim_max_visibility_miles) ~ as.numeric(upper_lim_max_visibility_miles), TRUE ~ max_visibility_miles)) %>% 
  mutate(max_visibility_miles = case_when(max_visibility_miles < as.numeric(lower_lim_max_visibility_miles) ~ as.numeric(lower_lim_max_visibility_miles), TRUE ~ max_visibility_miles))

#not removed for mean_visibility_miles due to outer and lower bounds being the same 
quartile_mean_visibility_miles <- quantile(weather_df_2$mean_visibility_miles, probs=c(.25, .75), na.rm = T)
iqr_mean_visibility_miles <- IQR(weather_df_2$mean_visibility_miles, na.rm=T)
upper_lim_mean_visibility_miles <- quartile_mean_visibility_miles[2] + 1.5*iqr_mean_visibility_miles
lower_lim_mean_visibility_miles <- quartile_mean_visibility_miles[1] - 1.5*iqr_mean_visibility_miles 
#outlier removal based on percentiles:
upper_lim_mean_visibility_miles <- quantile(weather_df_2$mean_visibility_miles, 0.975, na.rm=T)
lower_lim_mean_visibility_miles <- quantile(weather_df_2$mean_visibility_miles, 0.025, na.rm=T)
weather_df_2 <- weather_df_2 %>% 
  mutate(mean_visibility_miles = case_when(max_visibility_miles > as.numeric(upper_lim_mean_visibility_miles) ~ as.numeric(upper_lim_mean_visibility_miles), TRUE ~ mean_visibility_miles)) %>% 
  mutate(mean_visibility_miles = case_when(max_visibility_miles < as.numeric(lower_lim_mean_visibility_miles) ~ as.numeric(lower_lim_mean_visibility_miles), TRUE ~ mean_visibility_miles))

#outliers removed for max_wind_Speed_mph due to outer and lower bounds being different 
quartile_max_wind_speed_mph <- quantile(weather_df_2$max_wind_Speed_mph, probs=c(.25, .75), na.rm = T)
iqr_max_wind_Speed_mph <- IQR(weather_df_2$max_wind_Speed_mph, na.rm=T)
upper_lim_max_wind_Speed_mph <- quartile_max_wind_speed_mph[2] + 1.5*iqr_max_wind_Speed_mph
lower_lim_max_wind_Speed_mph <- quartile_max_wind_speed_mph[1] - 1.5*iqr_max_wind_Speed_mph 
#recoding of outliers  for max_wind_Speed_mph variable to upper IQR limit if higher-than upper limit, else recoding to lower limit 
weather_df_2 <- weather_df_2 %>%
  mutate(max_wind_Speed_mph = case_when(max_wind_Speed_mph > as.numeric(upper_lim_max_wind_Speed_mph) ~ as.numeric(upper_lim_max_wind_Speed_mph), TRUE ~ max_wind_Speed_mph)) %>% 
  mutate(max_wind_Speed_mph = case_when(max_wind_Speed_mph < as.numeric(lower_lim_max_wind_Speed_mph) ~ as.numeric(lower_lim_max_wind_Speed_mph), TRUE ~ max_wind_Speed_mph))

#outliers removed for max_gust_speed_mph due to outer and lower bounds being different 
quartile_max_gust_speed_mph <- quantile(weather_df_2$max_gust_speed_mph, probs=c(.25, .75), na.rm = T)
iqr_max_gust_speed_mph <- IQR(weather_df_2$max_gust_speed_mph, na.rm=T)
upper_max_gust_speed_mph <- quartile_max_gust_speed_mph[2] + 1.5*iqr_max_gust_speed_mph
lower_max_gust_speed_mph <- quartile_max_gust_speed_mph[1] - 1.5*iqr_max_gust_speed_mph 
#recoding of outliers  for max_gust_speed_mph variable to upper IQR limit if higher-than upper limit, else recoding to lower limit 
weather_df_2 <- weather_df_2 %>% 
  mutate(max_gust_speed_mph = case_when(max_gust_speed_mph > as.numeric(upper_max_gust_speed_mph) ~ as.numeric(upper_max_gust_speed_mph), TRUE ~ max_gust_speed_mph)) %>% 
  mutate(max_gust_speed_mph = case_when(max_gust_speed_mph < as.numeric(lower_max_gust_speed_mph) ~ as.numeric(lower_max_gust_speed_mph), TRUE ~ max_gust_speed_mph))

#outliers not removed for precipitation  due to outer and lower bounds being the same 
quartile_precipitation <- quantile(weather_df_2$precipitation_inches, probs=c(.25, .75), na.rm = T)
iqr_precipitation <- IQR(weather_df_2$precipitation_inches, na.rm=T)
upper_precipitation <- quartile_precipitation[2] + 1.5*iqr_precipitation
lower_precipitation <- quartile_precipitation[1] - 1.5*iqr_precipitation 
#outlier removal based on percentiles:
upper_precipitation <- quantile(weather_df_2$precipitation_inches, 0.975, na.rm=T)
lower_precipitation <- quantile(weather_df_2$precipitation_inches, 0.025, na.rm=T)
weather_df_2 <- weather_df_2 %>% 
  mutate(precipitation_inches = case_when(precipitation_inches > as.numeric(upper_precipitation) ~ as.numeric(upper_precipitation), TRUE ~ precipitation_inches)) %>% 
  mutate(precipitation_inches = case_when(precipitation_inches < as.numeric(lower_precipitation) ~ as.numeric(lower_precipitation), TRUE ~ precipitation_inches))

#Joining station to trip dataset to obtain details on city, will be used afterwards as a UID
#Merged df assigned to station_trip object
station_trip <- left_join(trip_clean2,station_df, by=c("start_station_name" = "name"))

#removing irrelevant variables from dataframe 
station_trip <- station_trip %>% select(-c(id.x, bike_id, start_station_id, 
                                       end_station_id, subscription_type, zip_code,id.y,installation_date,
                                       start_station_name,end_station_name, dock_count, long, lat))

#creating dataframe grouped by number of trips per day per city, as weather-variables were provided per day
trip_data_count <- station_trip %>% group_by(start_date, city) %>% count()

#joining the trip and weather data 
trip_weather_corr <- left_join(trip_data_count,weather_df_2, by=c("start_date" = "date", "city"))

#removing character-names after joining 
trip_weather_corr <- trip_weather_corr %>% select(-c(start_date,city))

#sf city 
san_fran <- as.data.frame(trip_weather_corr %>% filter(city == "San Francisco"))
attach(san_fran)
san_fran_1 <- san_fran %>% select(-c(start_date,city, events, zip_code))
san_fran_1 <- na.omit(san_fran_1)
san_fran_1 <- as.data.frame(san_fran_1)
san_fran_1$cloud_cover <- as.numeric(san_fran_1$cloud_cover)
#testing correlation values
corrplot(cor(san_fran_1))

#mountain view city 
mountain_view <- as.data.frame(trip_weather_corr %>% filter(city == "Mountain View"))
attach(mountain_view)
mountain_view_1 <- mountain_view %>% select(-c(start_date,city, events, zip_code))
mountain_view_1 <- na.omit(mountain_view_1)
mountain_view_1 <- as.data.frame(mountain_view_1)
mountain_view_1$cloud_cover <- as.numeric(mountain_view_1$cloud_cover)
mountain_view_1$zip_code <- as.numeric(mountain_view_1$zip_code)
#testing correlation values 
corrplot(cor(mountain_view_1))

#palo alto city 
palo_alto_view <- as.data.frame(trip_weather_corr %>% filter(city == "Palo Alto"))
attach(palo_alto_view)
palo_alto_view_1 <- palo_alto_view %>% select(-c(start_date,city, events, zip_code))
palo_alto_view_1 <- na.omit(palo_alto_view_1)
palo_alto_view_1 <- as.data.frame(palo_alto_view_1)
palo_alto_view_1$cloud_cover <- as.numeric(palo_alto_view_1$cloud_cover)
#testing correlation values 
corrplot(cor(palo_alto_view_1))

#Redwood City
redwood_city <- as.data.frame(trip_weather_corr %>% filter(city == "Redwood City"))
attach(redwood_city)
redwood_city_1 <- redwood_city %>% select(-c(start_date,city, events, zip_code))
redwood_city_1 <- na.omit(redwood_city_1)
redwood_city_1 <- as.data.frame(redwood_city_1)
redwood_city_1$cloud_cover <- as.numeric(redwood_city_1$cloud_cover)
#testing correlation values 
corrplot(cor(redwood_city_1))

#San Jose City
sanjose_city <- as.data.frame(trip_weather_corr %>% filter(city == "San Jose"))
attach(sanjose_city)
sanjose_city_1 <- sanjose_city %>% select(-c(start_date,city, events, zip_code))
sanjose_city_1 <- na.omit(sanjose_city_1)
sanjose_city_1 <- as.data.frame(sanjose_city_1)
sanjose_city_1$cloud_cover <- as.numeric(sanjose_city_1$cloud_cover)
#testing correlation values 
corrplot(cor(sanjose_city_1))

