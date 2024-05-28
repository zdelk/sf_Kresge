#Zachary Delk
#Southface
#
#Code that goes through a folder, pulls all the csv files, runs them through
#the combine variables function then summaries the data and output to an excel file

# Load the required package
#install.packages("openxlsx")
library(openxlsx)
library(statar)
library(readr)
library(dplyr)
library(lubridate)
# Set the directory containing the files
folder_path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Full Data"


# List all files in the folder with a specific extension (e.g., .csv)
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Initialize a list to store the summary statistics
summary_list <- list()

# Loop through each file
for (file_path in file_list) {
  # Read the file
  data <- read.csv(file_path)
  
  # Calculate summary statistics
  summary_data <- sum_up(combine_variables(data))
  
  # Add the summary to the list
  summary_list[[basename(file_path)]] <- summary_data
}

# Create a new Excel workbook
wb <- createWorkbook()

# Add each summary as a new worksheet in the Excel workbook
for (file_name in names(summary_list)) {
  addWorksheet(wb, sheetName = file_name)
  writeData(wb, sheet = file_name, x = summary_list[[file_name]])
}

# Save the Excel workbook
excel_file_path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Post_Summary_v2.xlsx"
saveWorkbook(wb, excel_file_path, overwrite = TRUE)

# Print a message
cat("Summary statistics saved to", excel_file_path, "\n")
#####################################################################################################
# #Creating a function
# folder_handler <- function(filepath, outputpath, execute){
# #Code that goes through a folder, pulls all the csv files, runs them through
# #the combine variables function then summaries the data and output to an excel file
# # Set the directory containing the files
# folder_path <- filepath
# 
# # List all files in the folder with a specific extension (e.g., .csv)
# file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# 
# # Initialize a list to store the summary statistics
# summary_list <- list()
# 
# # Loop through each file
# for (file_path in file_list) {
#   # Read the file
#   data <- read.csv(file_path)
#   
#   # Calculate summary statistics
#   summary_data <- execute
#   
#   # Add the summary to the list
#   summary_list[[basename(file_path)]] <- summary_data
# }
# 
# # Create a new Excel workbook
# wb <- createWorkbook()
# 
# # Add each summary as a new worksheet in the Excel workbook
# for (file_name in names(summary_list)) {
#   addWorksheet(wb, sheetName = file_name)
#   writeData(wb, sheet = file_name, x = summary_list[[file_name]])
# }
# 
# # Save the Excel workbook
# excel_file_path <- outputpath
# saveWorkbook(wb, excel_file_path, overwrite = TRUE)
# 
# # Print a message
# cat("Summary statistics saved to", excel_file_path, "\n")
# }
# ################################################################
# #Testing function
# folder_handler("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Full Data", 
#                "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Post_Do_Both_v4.xlsx",
#                do_both(combine_variables(data)))
# 
# albany_pre = by_hour_agg(Albany_pre_reno.csv)
# conyers_test = by
