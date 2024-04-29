#To Do:
#create dataset with average values across morning evening and night for each day in the data set

#going to need to collapse on the hour. 1:00 - 1:59 = 1
library(lubridate)

Albany_Post_Reno_v2.csv$date.hour = trunc(Albany_Post_Reno_v2.csv$Date.Time, units = "hours")

Albany_Post_Reno_v2.csv$date.hour2 = as.character(Albany_Post_Reno_v2.csv$date.hour)
hour_list = unique(as.character(Albany_Post_Reno_v2.csv$date.hour))

iaq_coloumns = c("Temp","Hum", "Dioxide","CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")


hour_means_matrix <- matrix(NA, nrow = length(hour_list), ncol = length(iaq_coloumns))

hour_means_matrix[,1] = hour_list

for (k in 1:length(iaq_coloumns)){
  name = iaq_coloumns[k]
  for (i in 1:length(hour_list)){
   hour_means_matrix[i,k] = mean(Albany_Post_Reno_v2.csv[Albany_Post_Reno_v2.csv$date.hour2 == hour_list[i], iaq_coloumns[k]])
  }
}
hour_list[1]


library(dplyr)
#####################################################################################
by_hour_agg = function(use_df) {
  ###Needs lubridate library
  #Converts time to POSIXlt (time) format and Strips the seconds from dateTime variable
  use_df$Date.Time = mdy_hm(use_df$DateTime, tz = Sys.timezone())
  #Further strips minutes from time data
  use_df$date.hour = trunc(use_df$Date.Time, units = "hours")
  #converts time back to character without hours and minutes
  use_df$date.hour2 = as.character(use_df$date.hour)
  #Creates a list with the just the unique times
  hour_list = unique(as.character(use_df$date.hour))
  #Creates a list with the values of the IAQ data columns
  iaq_coloumns = c("DateTime","Temp","Hum", "Dioxide","CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
  #Creates an empty matrix that is used in the for loop
  hour_means_matrix <- matrix(NA, nrow = length(hour_list), ncol = length(iaq_coloumns))
  #Places time in the first column of the new dataset
  hour_means_matrix[,1] = hour_list
  #Goes through the list of columns and rows and returns the average value of each
  #variables for each hour
  for (k in 2:length(iaq_coloumns)){
    name = iaq_coloumns[k]
    for (i in 1:length(hour_list)){
     hour_means_matrix[i,k] = mean(use_df[use_df$date.hour2 == hour_list[i], name])
    }
  }
  #Uses the other list above to rename all the columns in the new dataset
  colnames(hour_means_matrix) = iaq_coloumns
  
  return(hour_means_matrix)
}


#Test
Pt_hour = by_hour_agg(Peachtree_Post_Reno.csv)
albany_hour = by_hour_agg(Albany_Post_Reno_v2.csv)

use_df = Peachtree_Post_Reno.csv
use_df = East_Lake_Post_Reno_v2.csv


use_df$Date.Time = mdy_hm(use_df$DateTime, tz = Sys.timezone())
use_df$DateTest2 = ymd_hm(use_df$DateTime, tz = Sys.timezone(), truncated = 8)
use_df$DateTest = as.POSIXct(use_df$DateTime, tz = Sys.timezone(), tryFormats = c("%Y-%m-%d %H:%M", "%m/%d/%Y %H:%M"))

nzd$date <- format(as.Date(nzd$date, format = "%d/%m/%Y"), "%Y-%m-%d")
use_df$DateTest3 = format(as.Date(use_df$DateTime, format = "%Y-%m-%d %H:%M"), "%m/%d/%Y %H:%M")



#########################################
albany_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data NoSmoking/Albany_Post_Reno_v2.csv")

albany_data_full = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Albany_Post_Reno_v2.csv")

summary(albany_data)

albany_test = combine_variables(albany_data)
albany_full_test = combine_variables(albany_data_full)

summary(albany_test)

pre_albany = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PreReno/Full Data/Albany_pre_reno.csv")
pre_albany_v2 = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PreReno/Full Data/Albany_pre_v2.csv")

summary(pre_albany)

pre_albany_test = combine_variables(pre_albany)
pAT_v2 = combine_variables(pre_albany_v2)

summary(pre_albany_test)
library(statar)
sum_up(albany_test)

pre_albany_sub = subset(pre_albany_test, DateTime >= '2018-07-25 00:00:00')
pre_albany_sub_first = subset(pre_albany_test, DateTime <= '2018-05-9 00:00:00')

sum_up(pre_albany_sub)
sum_up(pre_albany_sub_first)
sum_up(albany_test)
sum_up(albany_full_test)
sum_up(pAT_v2)