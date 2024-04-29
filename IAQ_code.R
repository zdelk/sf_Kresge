#Zachary Delk
#5/1/2023
#For IAQ kresage data: SOUTHFACE
#
#######################################
#install.packages("lubridate")
#install.packages("openair")
#LOADING NECCESSARY LIBRARIES
library(readr) #READS .csv FILES
library(ggplot2) #EXTRA PLOTTING TOOLS
library(lubridate) #DOES DATE CONVERSTIONS
library("dplyr")  #EXTRA DATA MANIPULATION
library(openair) #DOES TIME AVERAGES
library(statar)
#######################################
sensor_list = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Pre_reno_sensor_full_test.csv")

albany_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Albany_Post_Reno.csv")

conyers_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/Conyers_Full_Data.csv")

east_lake_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/East_Lake_Full_Data.csv")

griffin_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Griffin_test_out_raw.csv")

milledge_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/milledgeville.csv")

lagrange_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Lucy Morgan/Lucy Morgan Pre-Reno Full Data .csv")

albany_data$avgCo2 = rowMeans(albany_data[,c(2:12)], na.rm =TRUE) 
albany_data$avgHum = rowMeans(albany_data[,c(13:23)], na.rm = TRUE)
albany_data$avgPM10 = rowMeans(albany_data[,c(24:34)], na.rm = TRUE)
albany_data$avgPM2.5 = rowMeans(albany_data[,c(35:45)], na.rm = TRUE)
albany_data$avgTemp = rowMeans(albany_data[,c(46:56)], na.rm = TRUE)

albany_avg = subset(albany_data, select = c(1,57:61))

albany_avg = subset(albany_data, select = c(1,(endsWith(colnames(albany_data), "CO2"))), filt)

test = albany_data %>% select(sample_id,)

albany_avg$DateTime = mdy_hm(albany_avg$DateTime,tz=Sys.timezone())

albany_avg$weekday = weekdays(albany_avg$DateTime)

albany_sumsup = as.data.frame(sum_up(albany_data))
###################################################

conyers_sumsup = as.data.frame(sum_up(conyers_data))

conyers_co2 = subset(conyers_data, select = 1)
conyers_co2 = conyers_data[, endsWith(colnames(conyers_data), "CO2")]

conyers_hum = subset(conyers_data, select = 1)
conyers_hum = conyers_data[, endsWith(colnames(conyers_data), "Humidity")]


conyers_temp = subset(conyers_data, select = 1)
conyers_temp = conyers_data[, endsWith(colnames(conyers_data), "Temperature")]

conyers_pm2.5 = subset(conyers_data, select = 1)
conyers_pm2.5 = conyers_data[, endsWith(colnames(conyers_data), "PM2.5")]

conyers_pm10 = subset(conyers_data, select = 1)
conyers_pm10 = conyers_data[, endsWith(colnames(conyers_data), "PM10")]


# create breaks
breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
labels <- c("Night", "Morning", "Afternoon", "Evening")



conyers_avg = subset(conyers_data, select = 1)
conyers_avg$DateTime = mdy_hm(conyers_avg$DateTime, tz = Sys.timezone())
conyers_avg$weekday = weekdays(conyers_avg$DateTime)
conyers_avg$weekend = "Weekday"
conyers_avg$weekend[conyers_avg$weekday == 'Saturday' | conyers_avg$weekday == 'Sunday'] = "Weekend"

conyers_avg$Time_of_day <- cut(x=hour(conyers_avg$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

conyers_avg$working = "Away"
conyers_avg$working[conyers_avg$Time_of_day == "Night" | conyers_avg$Time_of_day == "Evening"] = "Home"

conyers_avg$avgCo2 = rowMeans(conyers_co2[,c(2:10)], na.rm = TRUE) 
conyers_avg$avgHum = rowMeans(conyers_hum[,c(2:10)], na.rm = TRUE) 
conyers_avg$avgPM10 = rowMeans(conyers_pm10[,c(2:10)], na.rm = TRUE)
conyers_avg$avgPM2.5 = rowMeans(conyers_pm2.5[,c(2:10)], na.rm = TRUE)
conyers_avg$avgTemp = rowMeans(conyers_temp[,c(2:10)], na.rm = TRUE)


t.test(conyers_avg$avgPM2.5 ~ conyers_avg$working, conf.level = 0.95)

boxplot(conyers_avg$avgTemp ~ conyers_avg$Time_of_day, outline = FALSE)




# Scatter plot
plot(conyers_avg$avgCo2, conyers_avg$avgTemp, ylab = "HUM", xlab = "Co2", pch = 19,
      col = "darkblue", las = 1)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")   # add grid
   

########################################################################
lagrange_sumup = as.data.frame(sum_up(lagrange_data))

lagrange_avg = subset(lagrange_data, select = 1)
lagrange_avg$avgHum = rowMeans(lagrange_data[,c(4:26)])
lagrange_avg$avgCo2 = rowMeans(lagrange_data[,c(27:49)])
lagrange_avg$

griffin_sumup = as.data.frame(sum_up(griffin_data))





milledge_5 = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/milledgeville_5.csv")
milledge_5_sumup = as.data.frame(sum_up(milledge_5))


albany_18 = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/albany_18.csv")
albany_18_sumup = as.data.frame(sum_up(albany_18))



lagrange_8 = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Full Data Sets/lagrange_8.csv")
lagrange_8_sumup = as.data.frame(sum_up(lagrange_8))


summary(milledge_data)

full_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/5. Senseware IAQ Equipment Tracking and Data/Pre_reno_sensor_full_test.csv")


t.test(full_data$Avg.PM2.5 ~ full_data$Complex.Type, conf.level = 0.95)

boxplot(full_data$Avg.PM2.5~ full_data$Bedrooms, outline = FALSE)

table(full_data$Bedrooms)
albany_avg$weekend = "Weekday"
albany_avg$weekend[albany_avg$weekday == 'Saturday' | albany_avg$weekday == 'Sunday'] = "Weekend"


albany_avg$Time_of_day <- cut(x=hour(albany_avg$DateTime), breaks = breaks, labels = labels, include.lowest=TRUE)

albany_avg$working = "Away"
albany_avg$working[albany_avg$Time_of_day == "Night" | albany_avg$Time_of_day == "Evening"] = "Home"


t.test(data$unit1_test ~ data$section, conf.level = 0.95)



t.test(albany_avg$avgPM10 ~ albany_avg$working, conf.level = 0.95)

boxplot(albany_avg$avgHum ~ albany_avg$, outline = FALSE)
############################################################
############################################################
############################################################
install.packages("mice")

library(mice)

sensor_numeric <- sensor_list %>%
  select(Bedrooms, Blower.Test, Avg.Co2, Avg.Hum., Avg.Pm10, Avg.PM2.5, Avg.Temp)

md.pattern(sensor_numeric)

mice_imputed <- data.frame(
  original = sensor_list$Bedrooms,
  imputed_pmm = complete(mice(sensor_numeric, method = "pmm"))$Bedrooms,
  imputed_cart = complete(mice(sensor_numeric, method = "cart"))$Bedrooms,
  imputed_lasso = complete(mice(sensor_numeric, method = "lasso.norm"))$Bedrooms
)
mice_imputed

