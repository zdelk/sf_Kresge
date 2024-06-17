#Zachary Delk
#Southface
#Kresge Project
#Plotting script used for final write-up
#------------------------------------------------------------------------------#
# Installing pacakages if needed
#install.packages('tidyr')
#------------------------------------------------------------------------------#
# Loading libraries used
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
#------------------------------------------------------------------------------#
# Loading in data section

#Loading in the excel worksheet that has PostReno data
path_post <- "~/Kresge/Data/PostReno/Post_do_both_5_31.xlsx"
my_sheets = multiplesheets(path_post)
#All the variables present in Post Reno data collection
similar_columns_post <- c("Temp", "Hum", "Dioxide", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
#Combines all the Post Reno datasets
post_reno <- combine_similar_columns(my_sheets, similar_columns_post)

#Loading in the excel worksheet that has PreReno data
path_pre <- "~/Kresge/Data/PreReno/Prer_do_both_5_31.xlsx"
my_sheets = multiplesheets(path_pre)
#All the variables present in the Pre Reno data
similar_columns_pre <- c("Temp", "Hum", "CO2", "PM2.5", "PM10")
#Combines all the Pre Reno datasets
pre_reno <- combine_similar_columns(my_sheets, similar_columns_pre)

merged_df <- merge(post_reno, pre_reno, by = "ID")
#------------------------------------------------------------------------------#
# First set of long subsets

#Making long subsets for the Pre v Post Comparison
temp_long_subset <- long_subset_maker("Temp",merged_df)
hum_long_subset <- long_subset_maker("Hum", merged_df)
co2_long_subset <- long_subset_maker("CO2|Dioxide", merged_df) #Pre uses Dioxide Post uses CO2
pm2.5_long_subset <- long_subset_maker("PM2.5", merged_df)
pm10_long_subset <- long_subset_maker("PM10", merged_df)
#------------------------------------------------------------------------------#
# Setting up a plot preset for all the graphs needed

my_colors <- c("red", "blue", "green", "#FF7F00", "purple", "yellow","#F781BF")

# Set linetype based on the Type column
comp_plot <- function(long_ss, threshold, var_name){
  ggplot(long_ss, aes(x = ID, y = Test_var, color = Location, linetype = Type)) +
    geom_line(size = 1) +
    ylab(var_name)+
    #Adds a threshold line
    geom_hline(yintercept = threshold, linetype = "dashed", color = "black") +
    labs(color = "Location") +
    # theme(legend.text=element_text(size=rel(1.5)), legend.position = 'inside', 
    #       legend.position.inside = leg_pos)+
    scale_color_manual(values = my_colors) + #Each Location has its on color
    scale_linetype_manual(values = c("solid", "dashed"))+ #Post data has a solid line. Pre has a dashed line
    #Changes the x-axis to make more sense. Uses Days and has minor breaks every 12 hours
    scale_x_continuous(name = NULL, limits = c(0,336), expand = c(0,0) ,breaks = seq(0, 336, by = 24),
                       labels = paste0("Day ", c(0:14)), minor_breaks = seq(0, 336, by = 12))+
    theme_minimal(base_size = 20) #Gives a cleaner graph
}
#------------------------------------------------------------------------------#
# All The Pre v Post comparison graphs
comp_plot(temp_long_subset, 74, "Temperature (\u00B0F)")
comp_plot(hum_long_subset, 70, "Humidity (%)")
comp_plot(co2_long_subset, 2001, "Carbon Dioxide (ppm)")
comp_plot(pm2.5_long_subset, 55.5, expression("PM2.5 (" * mu * "g/m"^3 * ")"))
comp_plot(pm10_long_subset, 205, expression("PM10 (" * mu * "g/m"^3 * ")"))

#--------------------------------------------------------#
# This section is the Post Reno variables that have no Pre Reno comparison
hcho_long_subset <- long_subset_maker("HCHO", merged_df)
comp_plot(hcho_long_subset, 0.1, expression("Formalydhyde (mg/m"^3 * ")"))

co_long_subset <- long_subset_maker("Monoxide", merged_df)
comp_plot(co_long_subset, 0.9, "Carbon Monoxide (ppm)")

voc_long_subset <- long_subset_maker("Organic", merged_df)
comp_plot(voc_long_subset, 2001, "Volatile Organic Compounds (ppb)")
#------------------------------------------------------------------