library(readr)
library(readxl)

griffin_data = read.csv("C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PostReno/Raw Data/Griffin_test_out_raw.csv")

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


# specifying the path name 
path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PreReno/Pre_Combined_5_1.xlsx"
my_sheets = multiplesheets(path)

Map(assign, names(my_sheets), my_sheets, pos = 1)

library(lubridate)

Albany_Post_Reno_v2.csv$Date.Time = mdy_hm(Albany_Post_Reno_v2.csv$DateTime, tz = Sys.timezone())
Albany_Post_Reno_v2.csv$hour_test = mdy_h(Albany_Post_Reno_v2.csv$DateTime, tz = Sys.timezone())

for (col in colnames(Albany_Post_Reno_v2.csv[, -c(1)])) {
  plot(Albany_Post_Reno_v2.csv[[col]], Albany_Post_Reno_v2.csv$DateTime)
}

#Test Stuff##################
library(ggplot2)
plot(Albany_Post_Reno_v2.csv$Date.Time, Albany_Post_Reno_v2.csv$Temp)

ggplot(Albany_Post_Reno_v2.csv, aes(x = Date.Time, y = PM2.5)) +
  geom_line()

summary(Albany_Post_Reno_v2.csv)

conyers_hour = by_hour_agg(Conyers_Post_Reno_v2.csv)
summary(conyers_hour)

plot(conyers_hour$DateTime, conyers_hour$Temp)

conyers_hour$Date.Time = mdy_h(conyers_hour$DateTime, tz = Sys.timezone(), format = ("%Y-%m-%d %H:%M"))
conyers_hour$Date.Time = as.POSIXct(conyers_hour$DateTime, tz = "", format = ("%Y-%m-%d %H:%M"))
summary(conyers_hour)
conyers_hour$DateTest = as.Date(conyers_hour$DateTime, format = ("%Y-%m-%d %H:%M"))

plot(conyers_hour$Date.Time, conyers_hour$Temp)


ggplot(conyers_hour, aes(x = Date.Time, y = Temp)) +
  geom_line()
#################################################################
# Create a sequence of dates at hourly intervals from min_date to max_date


path <- "C:/Users/zdelk/OneDrive - Southface/Documents/Kresge/Data/PreReno/Combined_Pre_v2.xlsx"
my_sheets = multiplesheets(path)

Map(assign, names(my_sheets), my_sheets, pos = 1)

eL_hours = by_hour_agg(East_Lake_Post_Reno.csv)
albany_hours = by_hour_agg(Albany_pre_reno.csv)
conyers_hour = by_hour_agg(Conyers_Post_Reno_v2.csv)




summary(conyers_hour)
min_date = conyers_hour$Date.Time[1]
max_date = conyers_hour$Date.Time[336]
time_sequence <- seq(from = min_date, to = max_date, by = "hour")


albany_hours$ID = seq(nrow(albany_hours))


ggplot(albany_hours, aes(x = ID, y = Temp)) +
  geom_line()

df_list = list(eL_hours, albany_hours)

all_id = unique(unlist(lapply(df_list, function(df) c (df$ID))))
min_id = min(all_id)
max_id = max(all_id)

temp_data_hour <- data.frame(ID = all_id)
temp_data_hour$Albany_Post = albany_hours$Temp
temp_data_hour$EL_Post = eL_hours$Temp
temp_data_hour$ID = seq.int(nrow(temp_data_hour))

total = merge(albany_hours, eL_hours, by = "ID")

file_list
by_hour_agg(Peachtree_Post_Reno.csv)
