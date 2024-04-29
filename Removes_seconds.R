#this file removes the seconds from the datetime column for all files in list

library(readr)
setwd('C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Albany/Albany IAQ Data/Albany Raw Data')

# library(readxl)
# test = read_excel("H:/Projects/Kresge/5. Senseware IAQ Equipment Tracking and Data/Milledgeville/Milledgeville Raw Data/Milledge Pre-Reno CO2 Data 10.31.2018 to 2.6.2019.xls")
# 
# 
# 
library(lubridate)
# 
# test$DateTime = mdy_hm(test$DateTime,tz=Sys.timezone())
# 
# thing = read.csv('new-Milledge Pre-Reno Temperature.csv')


setwd("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Peachtree Towers/CO2")

extension <- "csv"

fileNames <- Sys.glob(paste("*.", extension, sep = ""))

fileNumbers <- seq(fileNames)

for (fileNumber in fileNumbers) {

  newFileName <-  paste("new-",
                        sub(paste("\\.", extension, sep = ""), "", fileNames[fileNumber]),
                        ".", extension, sep = "")

  # read old data:
  sample <- read_csv(fileNames[fileNumber])
  # add one to every widget value in every file:

  sample$DateTime = mdy_hm(sample$DateTime,tz=Sys.timezone())

  # write old data to new files:
  write.table(sample,
              newFileName,
              append = FALSE,
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)

}


###############################################################################################
library(lubridate)

extension <- "csv"

fileNames <- Sys.glob(paste("*.xls"))

fileNumbers <- seq(fileNames)

for (fileNumber in fileNumbers) {
  
  newFileName <-  paste("new-",
                        sub(paste("\\.", extension, sep = ""), "", fileNames[fileNumber]),
                        ".", extension, sep = "")
  
  # read old data:
  sample <- read_excel(fileNames[fileNumber])
  # add one to every widget value in every file:
  
  sample$DateTime = mdy_hm(sample$DateTime,tz=Sys.timezone())
  
  # write old data to new files:
  write.table(sample,
              newFileName,
              append = FALSE,
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)
  
}




file_names <- dir("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Peachtree Towers/CO2") #where you have your files

your_data_frame <- do.call(rbind,lapply(file_names,read.csv))


library(data.table)  
files <- list.files(path = "C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Peachtree Towers/CO2",pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill = TRUE)



test = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Peachtree Towers/CO2/sf-iaq-22-unit-212-co2.csv")
test$DateTime = mdy_hms(test$DateTime,tz=Sys.timezone())
albany$DateTime = mdy_hm(albany$DateTime,tz=Sys.timezone())





install.packages("rio")

library(rio)

convert("Albany Pre-Reno CO2 Data 4.24.2018 to 8.8.2018.xls", "Albany_PreReno_Co2_Raw")