#EDA Weather

#Loading necessary libraries
library("tidyverse")
library("funModeling")
library("Hmisc")

#Uploading weather.dataset and assigning to weather_df object
weather_df <- read.csv("weather.csv")

#Overview on variables from each column and few observations
glimpse(weather_df)

#Profiling the data input 
status(weather_df)
#q_zeroes; quantity of zeroes are high in precipitation_inches data set, supported by p_zeros column 
#q_inf and p_na does not show any infinite-values 
#q_na is high in max_gust_speed_mph, representing 24.71% of data according to p_na
#for type of input:
  #date is in character-form, needs to be converted to date-type
  #precipitation-inches in character, should be integer
  #cloud-cover can be factor, only 9 unique values
  #events can be factor, the "" seen from glimpse-view can be converted to NA, and only 5 unique values
  #zipcode can turned to factor, only 5 unique values
  #city can be factor, only 5 unique values

#analyzing categorical data 
freq(weather_df)
#informs that precipitation_inches values of T represent 4% of the data, needs to be filtered out 
#for events, "" value represents 80.71% of data, needs to be filtered out

#analyzing numerical data 
plot_num(weather_df)
#unbalanced variables: max_visibility_miles, max_wind_speed, max_gust_speed_zip-code 
profiling_num(weather_df)
#shows high standard deviation for max_temperature_f and mean_temperature_f, as well as zip_code(can be ignored considering will be turned into character-type)
#variation coefficient shows that the min_visibility_miles, max_wind_Speed_mph, max_wind_Speed_mph, mean_wind_speed_mph and cloud_cover columns have standard-deviation
#values that represent high % of mean 
#for skewness, can see that max_visibility_miles, max_wind_Speed_mph and max_gust_speed_mph are associated with asymmetric distribution
  #positive-skew shown for mean_visibility_miles, max_visibility_miles, mean_visibility_miles, max_wind_Speed_mph, max_gust_speed_mph
  #negative skew shown for mean_temperature_f, min_temperature_f, min_visibility_miles columns
#high kurtosis values for max_visibility_miles, mean_visibility_miles, max_wind_Speed_mph, max_gust_speed_mph columns indicating greater presence of outliers
#high IQR range for max_temperature_f, mean_temperature_f, min_temperature_f variables as well as zip_code but can be ignored considering future transformation to character-type


describe(weather_df)
#although kurtosis-values showed potential high measure of outliers for mean_visibility_miles, 
#T is encoded as entry for precipitation_inches
#can see 1473 missing entries for events-data 