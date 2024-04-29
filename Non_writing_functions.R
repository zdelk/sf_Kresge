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
by_hour_agg = function(use_df) {
  ###Needs lubridate library##
  #Converts time to POSIXlt (time) format and Strips the seconds from dateTime variable
  # Define expected date-time format
  expected_format <- "%m/%d/%Y %I:%M %p"
  
  # Check if DateTime column is already in the expected POSIXt format (mdy_hm)
  use_df$DateTime <- tryCatch({
    as.POSIXlt(use_df$DateTime, format = expected_format, tz = Sys.timezone())
  }, error = function(e) {
    warning("Date-time parsing failed. Attempting alternative conversion.")
    parse_date_time(use_df$DateTime, orders = c("mdy_hm", "dmy_hm", "ymd_hm"))
  })
  
  #Further strips minutes from time data
  use_df$date.hour = trunc(use_df$DateTime, units = "hours")
  #converts time back to character without hours and minutes
  use_df$date.hour2 = as.character(use_df$date.hour)
  #Creates a list with the just the unique times
  hour_list = unique(as.character(use_df$date.hour))
  #Creates a list with the values of the IAQ data columns
  iaq_coloumns = c("Temp", "Hum", "Dioxide", "CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
  new_coloumns = c("DateTime", "Temp", "Hum", "Dioxide", "CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
  #Creates an empty matrix that is used in the for loop
  hour_means_matrix <- matrix(NA, nrow = length(hour_list), ncol = length(iaq_coloumns))
  #Places time in the first column of the new dataset
  new_dataframe = data.frame(Date = hour_list)
  #Goes through the list of columns and rows and returns the average value of each
  #variables for each hour
  for (k in 1:length(iaq_coloumns)){
    name = iaq_coloumns[k]
    for (i in 1:length(hour_list)){
      hour_means_matrix[i,k] = mean(use_df[use_df$date.hour2 == hour_list[i], name])
    }
  }
  new_dataframe = cbind(new_dataframe, hour_means_matrix)
  #Uses the other list above to rename all the columns in the new dataset
  colnames(new_dataframe) = new_coloumns
  new_dataframe$ID = seq.int(nrow(new_dataframe))
  
  return(new_dataframe)
}
