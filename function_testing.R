#funciton testing

#Zacahry Delk######################
library(readr)
library(dplyr)

#imports the dataset
griffin_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Griffin_test_out_raw.csv")

#Creates a list of naming conventions to use later
test_list = c("Temp","Hum", "Dioxide", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide" )
#Creates another list that is used for creating a new dataset
new_col_names = c("DateTime","Temp","Hum", "Dioxide", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide" )


#Creates a duplicate of the earlier dataset to manipulate
use_df = griffin_data

#Creates a matrix that has the number of rows of the original data set and the columns from the naming convention list
row_means_matrix <- matrix(NA, nrow = nrow(use_df), ncol = length(test_list))

#creates a new data frame that only keeps the first column of the original data set. In this case the DateTime data
new_dataframe = use_df[, 1, drop = FALSE]

#For loop that iterates for each name in the naming convention list
for (i in 1:length(test_list)){
  #pulls the naming convention out of the list
  name = test_list[i]
    #Gets the row Mean of all columns that follow the current naming convention
    #Then fills the matrix
    row_means_matrix[,i] = rowMeans(select(use_df,contains(name)), na.rm = TRUE)
    
}

#Binds the DateTime data frame with the row matrix
new_dataframe = cbind(new_dataframe, row_means_matrix)
#Uses the other list above to rename all the columns in the new data set
colnames(new_dataframe) = new_col_names


summary(new_dataframe)

library(statar)

sum_up(new_dataframe)
####################################################################################################
#Subset every data frame to the first 336 data points (2 weeks,1 per hour)
#starting time should be noon for each data set



######################################################################################################
#Testing hour agg
by_hour_agg_pre = function(use_df) {
  ###Needs lubridate library
  
  #Testing
  use_df$dateConvert = as_datetime(use_df$DateTime)
  use_df$dateReform = format(use_df$dateConvert, "%m/%d/%Y %H:%M")
  
  #Converts time to POSIXlt (time) format and Strips the seconds from dateTime variable
  use_df$Date.Time = mdy_hm(use_df$dateReform, tz = Sys.timezone())
  #Further strips minutes from time data
  use_df$date.hour = trunc(use_df$Date.Time, units = "hours")
  #converts time back to character without hours and minutes
  use_df$date.hour2 = as.character(use_df$date.hour)
  #Creates a list with the just the unique times
  hour_list = unique(as.character(use_df$date.hour))
  #Creates a list with the values of the IAQ data columns
  iaq_coloumns = c("DateTime","Temp","Hum","CO2", "PM2.5", "PM10")
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
############
#Test by hour edit
albany_by_hour <- by_hour_agg_pre(Albany_pre_v2.csv)

albay_pre_test <- read.csv("~/Kresge/Data/PreReno/Full Data/Albany_pre_v2.csv")

abh_test <- by_hour_agg_pre(combine_variables(albay_pre_test))
conyers_test <- by_hour_agg_pre(Conyers_Full_Data.csv)
conyers_test2 <- by_hour_agg(Conyers_Full_Data.csv)

use_df = East_Lake_Full_Data.csv
library(ludate)

summary(use_df)
use_df$dateConvert = as_datetime(use_df$DateTime)
use_df$dateReform = format(use_df$dateConvert, "%m/%d/%Y %H:%M")

use_df$dateConvert = mdy_hm(use_df$DateTime)
use_df$dateReform = format(use_df$dateConvert, "%m/%d/%Y %H:%M")

convert_date <- function(df) {
  tryCatch(
    {
      df$DateConvert <- mdy_hm(df$DateTime)
      #df$DateConvert <- as_datetime(df$DateTime)
      print("mdy_hm")
    },
    warning = function(w) {
      tryCatch(
        {
          df$DateConvert <- as_datetime(df$DateTime)
          #df$DateConvert <- mdy_hm(df$DateTime)
          print("as_datetime")
        },
        warning = function(w) {
          cat("Error: Unable to convert date column using any method.")
          df$DateConvert <- NA
        }
      )
    }
  )
  return(df)
}
test_date_conv_conyers <- convert_date(Conyers_Full_Data.csv)
test_date_conv_albany <- convert_date(Albany_pre_reno.csv)


library(lubridate)

convert_date_2 <- function(df) {
  tryCatch(
    {
      df$DateConvert <- as_datetime(df$DateTime)
      print("as_datetime")
    },
    warning = function(w) {
      tryCatch(
        {
          df$DateConvert <- mdy_hm(df$DateTime)
          print("mdy_hm")
        },
        error = function(e) {
          cat("Error: Unable to convert date column using any method.")
          df$DateConvert <- NA
        }
      )
    },
    error = function(e) {
      cat("Error: Unable to convert date column using any method.")
      df$DateConvert <- NA
    }
  )
  return(df)
}

test_date_conv_albany_3 <- convert_date_2(Albany_pre_reno.csv)
test_date_conv_conyers_2 <- convert_date_2(Conyers_Full_Data.csv)
test_4 = Conyers_Full_Data.csv
####################################################################

