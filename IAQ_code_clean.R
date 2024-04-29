#Zachary Delk
#5/1/2023
#For IAQ kresage data: SOUTHFACE
#
#######################################
#LOADING NECCESSARY LIBRARIES
library(readr) #READS .csv FILES
library(ggplot2) #EXTRA PLOTTING TOOLS
library(lubridate) #DOES DATE CONVERSTIONS
library("dplyr")  #EXTRA DATA MANIPULATION
library(openair) #DOES TIME AVERAGES
library(statar)
library(do)
#######################################

sensor_list = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Pre_reno_sensor_full.csv")

#albany_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Albany_Full_Data.csv")

#iaq_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/iaq_Full_Data.csv")

east_lake_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/East_Lake_Full_Data.csv")

iaq_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Griffin_Full_Data.csv")

#milledge_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/milledgeville.csv")

#lagrange_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Lucy Morgan/Lucy Morgan Pre-Reno Full Data .csv")

peachtree_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Peachtree_Tower_Full_Data.csv")

griffin_test_out = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Griffin_test_out_raw.csv")

griffin_pre_reno = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Griffin_Full_Data.csv")

peachtree_post_reno = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Peachtree_Post_Reno.csv")


iaq_data = peachtree_post_reno
###################################################
east_lake_sumup = as.data.frame(sum_up(east_lake_data))

peachtree_sumup = as.data.frame(sum_up(peachtree_data))

griffin_to_sumup = as.data.frame(sum_up(griffin_test_out))

write_xlsx(griffin_to_sumup, file = "Griffin_sumup.xlsx", sheetName = 'Sheet1')

griffin_pr_sumup = as.data.frame(sum_up(griffin_pre_reno))

write_xlsx(griffin_pr_sumup, file = "Griffin_Pre_Reno_sumup.xlsx", sheetName = 'Sheet1')

##################################################
iaq_sumsup = as.data.frame(sum_up(iaq_data))

iaq_co2 = subset(iaq_data, select = 1)
iaq_co2 = iaq_data[, endsWith(colnames(iaq_data), "CO..")]


iaq_co = subset(iaq_data, select = 1)
iaq_co = iaq_data[, endsWith(colnames(iaq_data), "CO.")]

iaq_hum = subset(iaq_data, select = 1)
iaq_hum = iaq_data[, endsWith(colnames(iaq_data), "Humidity")]


iaq_temp = subset(iaq_data, select = 1)
iaq_temp = iaq_data[, endsWith(colnames(iaq_data), "Temperature")]

iaq_pm2.5 = subset(iaq_data, select = 1)
iaq_pm2.5 = iaq_data[, endsWith(colnames(iaq_data), "PM2.5.")]

iaq_pm10 = subset(iaq_data, select = 1)
iaq_pm10 = iaq_data[, endsWith(colnames(iaq_data), "PM10.")]


iaq_hcho = subset(iaq_data, select = 1)
iaq_hcho = iaq_data[, endsWith(colnames(iaq_data), "HCHO.")]


iaq_voc = subset(iaq_data, select = 1)
iaq_voc = iaq_data[, endsWith(colnames(iaq_data), "VOC.")]


# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")



iaq_avg = subset(iaq_data, select = 1)
iaq_avg$DateTime = mdy_hm(iaq_avg$DateTime, tz = Sys.timezone())
iaq_avg$weekday = weekdays(iaq_avg$DateTime)
iaq_avg$weekend = "Weekday"
iaq_avg$weekend[iaq_avg$weekday == 'Saturday' | iaq_avg$weekday == 'Sunday'] = "Weekend"

iaq_avg$Time_of_day <- cut(x=hour(iaq_avg$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

iaq_avg$working = "Away"
iaq_avg$working[iaq_avg$Time_of_day == "Night" | iaq_avg$Time_of_day == "Evening"] = "Home"

iaq_avg$avgCo2 = rowMeans(iaq_co2, na.rm = TRUE) 
iaq_avg$avgHum = rowMeans(iaq_hum, na.rm = TRUE) 
iaq_avg$avgPM10 = rowMeans(iaq_pm10, na.rm = TRUE)
iaq_avg$avgPM2.5 = rowMeans(iaq_pm2.5, na.rm = TRUE)
iaq_avg$avgTemp = rowMeans(iaq_temp, na.rm = TRUE)
iaq_avg$avgCo = rowMeans(iaq_co, na.rm = TRUE)
iaq_avg$avgVoc = rowMeans(iaq_voc, na.rm = TRUE)
iaq_avg$avgHcho = rowMeans(iaq_hcho, na.rm = TRUE)


summary(iaq_avg)

###########################################################
#WEEKDAY VS WEEKEND
#install.packages('tidystats')
library(broom)
t.test(iaq_avg$avgCo2 ~ iaq_avg$weekend, conf.level = 0.95)

t.test(iaq_avg$avgHum ~ iaq_avg$weekend, conf.level = 0.95)

t.test(iaq_avg$avgPM10 ~ iaq_avg$weekend, conf.level = 0.95)

t.test(iaq_avg$avgPM2.5 ~ iaq_avg$weekend, conf.level = 0.95)

t.test(iaq_avg$avgTemp ~ iaq_avg$weekend, conf.level = 0.95)

###########################################################

t.test(iaq_avg$avgPM2.5 ~ iaq_avg$working, conf.level = 0.95)

boxplot(iaq_avg$avgTemp ~ iaq_avg$Time_of_day, outline = FALSE)

##################################################################
#Scatterplot
albany_pre_reno = read_csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PreReno/Full Data/Albany_pre_reno.csv")
albany_pre_reno$DateTime = as_datetime(albany_pre_reno$DateTime)

plot(albany_pre_reno$DateTime, albany_pre_reno$`SF-IAQ-15 - Temperature`, ylim = c(70,90))


albany_pre_full = combine_variables(albany_pre_reno)