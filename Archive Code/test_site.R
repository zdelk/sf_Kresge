#Zachary Delk
#3/14/2023
#For IAQ kresage data: SOUTHFACE
#
#TEST SITE
#
#USED TEST OUT DIFFERENT STUFF
#
#The following code is mean to explore the current day and look for 
# any statistical significance
#An attempt will be made to describe how the code works.
#######################################
#LOADING NECCESSARY LIBRARIES
library(readr) #READS .csv FILES
library(ggplot2) #EXTRA PLOTTING TOOLS
library(lubridate) #DOES DATE CONVERSTIONS
library("dplyr")  #EXTRA DATA MANIPULATION
library(openair) #DOES TIME AVERAGES
#######################################
#LOADING IN THE DATASET
albany = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Albany_Full_Data.csv")

#######################################
#########DATA FORMATTING###############
#######################################
#Converting datetime from character to POSIXct
albany$DateTime = mdy_hm(albany$DateTime,tz=Sys.timezone())

#creating a subset dataframe that removes the extra DateTime variables
df = subset(albany, select = -c(DateTime.1, DateTime.2, DateTime.3, DateTime.4)) 
#We want a subset to mess with. 
#We will also remove the extra date column

colnames(df)[1] = 'date' 
#(necessary) timeAverage needs the date data to be named 'date'

df$weekday = weekdays(df$date) 
#(USEFUL) using a base R function to make a new column
#that converts DATETIME data to day of the week

#rowMeans will get the average across the columns
#We section them of to each sensor data type
df$avgCo2 = rowMeans(df[,c(2:12)], na.rm = TRUE) 
#selecting all the Co2 data columns and 
#and returning the mean of all the values. also ignores missing values

df$avgHum = rowMeans(df[,c(13:23)], na.rm =TRUE) 

df$avgPm10 = rowMeans(df[,c(24:34)], na.rm = TRUE)

df$avg2.5 = rowMeans(df[,c(35:45)], na.rm = TRUE)

df$avgTemp = rowMeans(df[,c(46:56)], na.rm = TRUE)

test_stuff = timeAverage(df, avg.time = "day") 
#(openair) Day averages for each sensor and averages

# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")

df$Time_of_day <- cut(x=hour(df$date), breaks = breaks, labels = labels,
                      include.lowest=TRUE)
#test_stuff$weekday = weekdays(test_stuff$date)

#######################################
########DATA ANALYSIS##################
#######################################
aggregate(x = test_stuff[,c(57:61)],  #(test) gets the average of one column 
          by = list(test_stuff$weekday),         #across the day of the week
          FUN = mean, 
          na.rm = TRUE)

aggregate(x = df[,c(58:62)],  #(test) gets the average of one column 
          by = list(df$Time_of_day),     #across the day of the week
          FUN = mean, 
          na.rm = TRUE)

model = lm(test_stuff$date ~ test_stuff$avgTemp) #linear model

#######################################
########DATA VISUALIZATION#############
#######################################
#stratified boxplot for Avg PM2.5 by weekday (no outliers)
boxplot(test_stuff$avg2.5~ test_stuff$weekday, outline = FALSE)

#scatterplot
plot(test_stuff$date, test_stuff$avgTemp)
#adding trendline
abline(lm(test_stuff$avgTemp ~ test_stuff$date))

#stratified boxplot for Avg PM2.5 by time of day (no outliers)
boxplot(df$avg2.5~ df$Time_of_day, outline = FALSE)


