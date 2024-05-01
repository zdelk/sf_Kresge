#Used for cleaning data sets with empty columns

library(readr)

to_clean = read_csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Albany/Albany IAQ Data/Albany Raw Data/Albany Pre-Reno CO2 Data.csv")

empty_columns = colSums(is.na(to_clean)| to_clean == "") == nrow(to_clean)

empty_columns

to_clean = to_clean[, !empty_columns]



setwd('C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Albany/Albany IAQ Data/Albany Raw Data')

# 
# 
library(lubridate)

extension <- "csv"

fileNames <- Sys.glob(paste("*.", extension, sep = ""))

fileNumbers <- seq(fileNames)

for (fileNumber in fileNumbers) {
  
  newFileName <-  paste("cleaned-",
                        sub(paste("\\.", extension, sep = ""), "", fileNames[fileNumber]),
                        ".", extension, sep = "")
  
  # read old data:
  sample <- read.csv(fileNames[fileNumber],
                     header = TRUE,
                     sep = ","
  )
  # add one to every widget value in every file:
  
  
  empty_columns = colSums(is.na(sample)| sample == "") == nrow(sample)
  
  sample = sample[, !empty_columns]
  
  # write old data to new files:
  write.table(sample,
              newFileName,
              append = FALSE,
              quote = FALSE,
              sep = ",",
              row.names = FALSE,
              col.names = TRUE)
  
}
