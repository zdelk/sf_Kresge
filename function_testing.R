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



library(lubridate)

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

#Test convert_date in by_hour_agg

by_hour_agg_test = function(use_df) {
  ###Needs lubridate library
  
  #Testing
  suppressWarnings({
    use_df$dateConvert <- as_datetime(use_df$DateTime)
  })
  
  
  # Check if conversion was successful
  if(all(!is.na(use_df$dateConvert))) {
    #print("as_datetime")
  } else {
    # Attempt conversion with mdy_hm if as_datetime fails
    use_df$dateConvert <- mdy_hm(use_df$DateTime)
    
    # Check if conversion was successful
    if(all(!is.na(use_df$dateConvert))) {
      #print("mdy_hm")
    } else {
      # If both conversions fail, print error message
      cat("Error: Unable to convert date column using any method.")
    }
  }
  
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

albany_hour_test <- by_hour_agg(Albany_Post_Reno_v2.csv)
albany_date_test <- convert_date(Albany_Post_Reno_v2.csv)

#############################################################################
use_df <- Milledgeville_Post_Reno.csv

by_hour_agg_test = function(use_df) {
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

do_both <- function(use_df){
  df <- by_hour_agg_test(convert_date(use_df))
  return(df)
}

test_both2 <- by_hour_agg_test(convert_date(Albany_Post_Reno_v2.csv))
test_both3 <- do_both(Albany_Post_Reno_v2.csv)
test_both4 <- do_both(Albany_pre_reno.csv)
test_both6 <- do_both(Conyers_Full_Data.csv)

ncol(Conyers_Full_Data.csv)

colnames(Conyers_Full_Data.csv)
test_list <- colnames(Conyers_Full_Data.csv)
####################################################################
#Testing subset to 2 weeks

path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Post_do_both_v2.xlsx"
my_sheets = multiplesheets(path)

for(i in 1:length(my_sheets)){
  use_df <- my_sheets[[i]]
  use_df$hour <- format(as_datetime(use_df$DateTime), format = "%H")
  start_num <- as.numeric(match("12",use_df$hour))
  end_num <- start_num + 336
  
  use_df <- use_df[start_num:end_num,]
  use_df$ID <- seq.int(nrow(use_df))
  my_sheets[[i]] <- use_df
  my_sheets[[i]][2:10] <- sapply(my_sheets[[i]][2:10], as.numeric)
}


path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Post_do_both_v2.xlsx"
my_sheets = multiplesheets(path)
Map(assign, names(my_sheets), my_sheets, pos = 1)
var_names <- c("Temp","Hum", "Dioxide","CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
post_names = c("ID","Post_Albany", "Post_Conyers", "Post_EL", "Post_Griffin", "Post_Milledge","Post_PT")

plot_df <- matrix(NA, nrow = 337, ncol = length(post_names))
plot_df[,1] <- seq.int(nrow(plot_df))

for(i in 1:length(my_sheets)){
  use_df <- my_sheets[[i]]
  plot_df[,i+1] <- use_df[,2]
}


colnames(plot_df) = post_names
plot_df <- as.data.frame(plot_df)
use_df <- my_sheets[[1]]
plot_df[,2] <- use_df[,2]

plot_list <- list()
for(i in 1:length(var_names)){
  var_names[i] <- matrix(NA, nrow = 337, ncol = length(post_names))
  plot_list <- append(plot_list, var_names[i],0)
}

var_names[1] <- matrix(NA, nrow = 337, ncol = length(post_names))


###########################################################################
combine_similar_columns <- function(df_list, similar_columns) {
  # Initialize an empty data frame to store combined columns
  combined_df <- data.frame(matrix(NA, nrow = 337, ncol = 1))
  combined_df[1] <- seq.int(nrow(combined_df))
  
  namelist = names(df_list)
  namelist_clean <- gsub("\\.csv$", "", namelist)
  
  # Iterate over each similar column
  for (col in similar_columns) {
    # Initialize a list to store column data from each data frame
    column_data <- list()
    
    # Iterate over each data frame in the list
    for (i in seq_along(df_list)) {
      # Check if the column exists in the current data frame
      if (col %in% names(df_list[[i]])) {
        # If the column exists, add it to the column data list
        column_data[[i]] <- df_list[[i]][[col]]
      } else {
        # If the column doesn't exist, add NA values to maintain alignment
        column_data[[i]] <- rep(NA, nrow(df_list[[i]]))
      }
    }
    
    # Combine the columns using cbind
    combined_column <- do.call(cbind, column_data)
    
    # Set column names for the combined column

    colnames(combined_column) <- paste0(namelist_clean, "_", col)
    
    
    # Add the combined column to the combined data frame
    combined_df <- cbind(combined_df, combined_column)
  }
  
  return(combined_df)
}



# Call the function to create the new data frame
path_post <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Post_do_both_v3.xlsx"
my_sheets = multiplesheets(path_post)
similar_columns_post <- c("Temp", "Hum", "Dioxide", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")

for(i in 1:length(my_sheets)){
  use_df <- my_sheets[[i]]
  use_df$hour <- format(as_datetime(use_df$DateTime), format = "%H")
  start_num <- as.numeric(match("12",use_df$hour))
  end_num <- start_num + 336
  
  use_df <- use_df[start_num:end_num,]
  use_df$ID <- seq.int(nrow(use_df))
  my_sheets[[i]] <- use_df
  my_sheets[[i]][2:10] <- sapply(my_sheets[[i]][2:10], as.numeric)
}

post_reno <- combine_similar_columns(my_sheets, similar_columns_post)
colnames(post_reno)[1] <- "ID"




path_pre <- "~/Kresge/Data/PreReno/Prer_do_both_v3.xlsx"
my_sheets = multiplesheets(path_pre)
similar_columns_pre <- c("Temp", "Hum", "CO2", "PM2.5", "PM10")



for(i in 1:length(my_sheets)){
  use_df <- my_sheets[[i]]
  use_df$hour <- format(as_datetime(use_df$DateTime), format = "%H")
  start_num <- as.numeric(match("12",use_df$hour))
  end_num <- start_num + 336
  
  use_df <- use_df[start_num:end_num,]
  use_df$ID <- seq.int(nrow(use_df))
  my_sheets[[i]] <- use_df
  my_sheets[[i]][2:10] <- sapply(my_sheets[[i]][2:10], as.numeric)
}


pre_reno <- combine_similar_columns(my_sheets, similar_columns_pre)
colnames(pre_reno)[1] <- "ID"

merged_df <- merge(post_reno, pre_reno, by = "ID")



namelist = names(my_sheets)
namelist_clean <- gsub("\\.csv$", "", namelist)
full_list = c("ID", namelist_clean)

library(ggplot2)
library(tidyr)
#################################################
#Temp
temp_subset <- subset(merged_df, select = c("ID", grep("Temp", names(merged_df), value = TRUE)))

colClean <- function(x){ colnames(x) <- gsub("\\_Temp+|_v2|_Data", "", colnames(x)); x } 

temp_subset <- colClean(temp_subset)

temp_names <- names(temp_subset[-1])

temp_subset_long <- pivot_longer(temp_subset, 
                                 cols = all_of(temp_names), 
                                 names_to = "variable", 
                                 values_to = "temperature")

ggplot(temp_subset_long, aes(x = ID, y = temperature, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red") +
  labs(color = "Location")


#############################################################################
#Humidity
hum_subset <- subset(merged_df, select = c("ID", grep("Hum", names(merged_df), value = TRUE)))

colClean <- function(x){ colnames(x) <- gsub("\\_Hum+|_v2|_Data", "", colnames(x)); x } 

hum_subset <- colClean(hum_subset)

hum_names <- names(hum_subset[-1])

hum_subset_long <- pivot_longer(hum_subset, 
                                 cols = all_of(hum_names), 
                                 names_to = "variable", 
                                 values_to = "humidity")

ggplot(hum_subset_long, aes(x = ID, y = humidity, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 70, linetype = "dashed", color = "red") +
  labs(color = "Location")
#############################################################################
#CO2
co2_subset <- subset(merged_df, select = c("ID", grep("CO2|Dioxide", names(merged_df), value = TRUE)))

colClean <- function(x){ colnames(x) <- gsub("\\_CO2+|_Dioxide|_v2|_Data", "", colnames(x)); x } 

co2_subset <- colClean(co2_subset)

co2_names <- names(co2_subset[-1])

co2_subset_long <- pivot_longer(co2_subset, 
                                 cols = all_of(co2_names), 
                                 names_to = "variable", 
                                 values_to = "CO2")

ggplot(co2_subset_long, aes(x = ID, y = CO2, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 2001, linetype = "dashed", color = "red") +
  labs(color = "Location")


#############################################################################
#PM2.5
pm25_subset <- subset(merged_df, select = c("ID", grep("PM2.5", names(merged_df), value = TRUE)))

colClean <- function(x){ colnames(x) <- gsub("\\_PM2.5|_v2|_Data", "", colnames(x)); x } 

pm25_subset <- colClean(pm25_subset)

pm25_names <- names(pm25_subset[-1])

pm25_subset_long <- pivot_longer(pm25_subset, 
                                cols = all_of(pm25_names), 
                                names_to = "variable", 
                                values_to = "PM2.5")

ggplot(pm25_subset_long, aes(x = ID, y = PM2.5, color = variable)) +
  geom_line() +
  geom_hline(yintercept = 55.5, linetype = "dashed", color = "red") +
  labs(color = "Location")

#############################################################################
#PM10
pm10_subset <- subset(merged_df, select = c("ID", grep("PM10", names(merged_df), value = TRUE)))
colClean <- function(x){ colnames(x) <- gsub("\\_PM10+|_v2|_Data", "", colnames(x)); x } 
pm10_subset <- colClean(pm10_subset)

pm10_subset <- subset(pm10_subset, select =-c(Prer_Lucy_Morgan))



pm10_names <- names(pm10_subset[-1])

pm10_subset_long <- pivot_longer(pm10_subset, 
                                cols = all_of(pm10_names), 
                                names_to = "variable", 
                                values_to = "PM10")


my_colors <- c("red", "blue", "green", "black", "purple", "yellow")

# Create a new column indicating whether it's Pre or Post
pm10_subset_long$Type <- ifelse(grepl("Prer", pm10_subset_long$variable), "Pre", "Post")
pm10_subset_long$Location <- ifelse(grepl("Albany", pm10_subset_long$variable),"Albany",
                                          ifelse(grepl("Conyers", pm10_subset_long$variable), "Conyers",
                                                 ifelse(grepl("PT", pm10_subset_long$variable), "PT",
                                                        ifelse(grepl("Milledgeville", pm10_subset_long$variable), "Milledgeville",
                                                               ifelse(grepl("East_Lake", pm10_subset_long$variable), "East_Lake", "Griffin")))))



# Set linetype based on the Type column
ggplot(pm10_subset_long, aes(x = ID, y = PM10, color = Location, linetype = Type)) +
  geom_line(size = 1) +
  ggtitle("PM10 by Location")+
  geom_hline(yintercept = 255, linetype = "dashed", color = "red") +
  labs(color = "Location") +
  scale_color_manual(values = my_colors) +
  scale_linetype_manual(values = c("dashed", "solid"))

#####################################################################################

hcho_subset <- subset(merged_df, select = c("ID", grep("HCHO", names(merged_df), value = TRUE)))
colClean <- function(x){ colnames(x) <- gsub("\\_HCHO+|_v2|_Data", "", colnames(x)); x } 
hcho_subset <- colClean(hcho_subset)

#hcho_subset <- subset(hcho_subset, select =-c(Prer_Lucy_Morgan))



hcho_names <- names(hcho_subset[-1])

hcho_subset_long <- pivot_longer(hcho_subset, 
                                 cols = all_of(hcho_names), 
                                 names_to = "variable", 
                                 values_to = "HCHO")



# Set linetype based on the Type column
ggplot(hcho_subset_long, aes(x = ID, y = HCHO, color = variable)) +
  geom_line(size = 1) +
  ggtitle("HCHO by Location")+
  geom_hline(yintercept = 0.1, linetype = "dashed", color = "red") +
  labs(color = "Location") +
  scale_color_manual(values = my_colors) 

