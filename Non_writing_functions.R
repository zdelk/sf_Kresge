#Zachary Delk
#Southface
#All Created Functions
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
library(openxlsx)
library(statar)
#Reads all sheets in an excel file
multiplesheets <- function(fname) { 
  
  # getting info about all excel sheets 
  sheets <- readxl::excel_sheets(fname) 
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x)) 
  data_frame <- lapply(tibble, as.data.frame) 
  
  # assigning names to data frames 
  names(data_frame) <- sheets 
  
  # print data frame 
  print(data_frame) 
} 

#Format to use Above
# 
# path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Combined_Post_v2.xlsx"
# my_sheets = multiplesheets(path)
# 
# Map(assign, names(my_sheets), my_sheets, pos = 1)

##############################
#Creates a function that averages data across like columns
#Then returns a new data set
combine_variables = function(use_df){
  #Creates a list of naming conventions to use later
  test_list = c("Temp","Hum", "Dioxide","CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
  #Creates another list that is used for creating a new dataset
  new_col_names = c("DateTime","Temp","Hum", "Dioxide", "CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
  
  #Creates a matrix that has the number of rows of the orginal dataset and the coloums from the naming convention list
  row_means_matrix <- matrix(NA, nrow = nrow(use_df), ncol = length(test_list))
  
  #creates a new dataframe that only keeps the first coloumn of the orginal dataset. In this case the DateTime data
  new_dataframe = use_df[, 1, drop = FALSE]
  
  #For loop that iterates for each name in the naming convention list
  for (i in 1:length(test_list)){
    #pulls the naming convention out of the list
    name = test_list[i]
    #Gets the row Mean of all columns that follow the current naming convention
    #Then fills the matrix
    row_means_matrix[,i] = rowMeans(dplyr::select(use_df,contains(name)), na.rm = TRUE)
  }
  
  #Binds the DateTime dataframe with the row matrix
  new_dataframe = cbind(new_dataframe, row_means_matrix)
  #Uses the other list above to rename all the columns in the new dataset
  colnames(new_dataframe) = new_col_names
  
  return(new_dataframe)
}
##############################
#convert_date try two different methods to convert DateTime
#returns the same data frame with a DateConvert column
####used with by_hour_agg
convert_date <- function(df) {
  # Attempt conversion with as_datetime
  suppressWarnings({
    df$dateConvert <- as_datetime(df$DateTime)
  })
  
  # Check if conversion was successful
  if(all(!is.na(df$dateConvert))) {
    #print("as_datetime")
  } else {
    # Attempt conversion with mdy_hm if as_datetime fails
    df$dateConvert <- mdy_hm(df$DateTime)
    
    # Check if conversion was successful
    if(all(!is.na(df$dateConvert))) {
      #print("mdy_hm")
    } else {
      # If both conversions fail, print error message
      cat("Error: Unable to convert date column using any method.")
    }
  }
  
  return(df)
}
#########################
#Returns a data frame that has a single datapoint for
#each hour. Needs convert_date first
by_hour_agg = function(use_df) {
  ###Needs lubridate library
  
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
#####################################
#Does convert_date and by_hour_agg in the correct order
#needs a better name lol
do_both <- function(use_df){
  df <- by_hour_agg(convert_date(use_df))
  return(df)
}