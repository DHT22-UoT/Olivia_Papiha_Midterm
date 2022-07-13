#Outlier Removal for Weather

#loading libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(stats)

#reading in weather and data, assigning to weather_df object
weather_df <- readRDS("weather_transform.rds")

#removing NA values for unavailable dates, assigning to weather_df_2
weather_df_2 <- weather_df %>% filter(!is.na(date))
#EDA indicated presence of outliers in the following variables: max_visibility_miles, 
#mean_visibility_miles, max_wind_Speed_mph, max_gust_speed_mph and precipitation_inches 

#outliers being removed based on the IQR method 
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

saveRDS(weather_df_2, "weather_no_outliers.rds")
