
library(readxl)
humidity_Milledge = read_excel("H:/Projects/Kresge/5. Senseware IAQ Equipment Tracking and Data/Milledgeville/Milledge Pre-Reno Humidity Data 10.31.2018 to 3.6.2019.xlsx")

summary(humidity_Milledge)

complete_humidity_Milledge = humidity_Milledge[complete.cases(humidity_Milledge),]


set.seed(9999)
train_df = complete_humidity_Milledge[sample(nrow(complete_humidity_Milledge), size = 100),]
