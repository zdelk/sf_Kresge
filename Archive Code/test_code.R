#Kresge Post Reno Code
#Zacahry Delk
######################
library(readr)
library(dplyr)

griffin_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Griffin_test_out_raw.csv")

test_list = c("Temp", "Dioxide")

# Function to calculate average of columns with similar names
get_average <- function(data) {
  data %>%
    summarise(across(contains("Temp"), mean, na.rm = TRUE))
}

# Calculate averages
averages <- get_average(griffin_data)

# Print the result
print(averages)