library(readr)
library(readxl)
#install.packages('tidyr')
library(tidyr)
library(ggplot2)


path_post <- "~/Kresge/Data/PostReno/Post_do_both_5_31.xlsx"
my_sheets = multiplesheets(path_post)
similar_columns_post <- c("Temp", "Hum", "Dioxide", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
post_reno <- combine_similar_columns(my_sheets, similar_columns_post)


path_pre <- "~/Kresge/Data/PreReno/Prer_do_both_5_31.xlsx"
my_sheets = multiplesheets(path_pre)
similar_columns_pre <- c("Temp", "Hum", "CO2", "PM2.5", "PM10")
pre_reno <- combine_similar_columns(my_sheets, similar_columns_pre)


merged_df <- merge(post_reno, pre_reno, by = "ID")

temp_long_subset <- long_subset_maker("Temp",merged_df)
hum_long_subset <- long_subset_maker("Hum", merged_df)
co2_long_subset <- long_subset_maker("CO2|Dioxide", merged_df)
pm2.5_long_subset <- long_subset_maker("PM2.5", merged_df)
pm10_long_subset <- long_subset_maker("PM10", merged_df)

my_colors <- c("red", "blue", "green", "#FF7F00", "purple", "yellow","#F781BF")

# Set linetype based on the Type column
comp_plot <- function(long_ss, threshold, var_name){
  ggplot(long_ss, aes(x = ID, y = Test_var, color = Location, linetype = Type)) +
    geom_line(size = 1) +
    ylab(var_name)+
    geom_hline(yintercept = threshold, linetype = "dashed", color = "black") +
    labs(color = "Location") +
    # theme(legend.text=element_text(size=rel(1.5)), legend.position = 'inside', 
    #       legend.position.inside = leg_pos)+
    scale_color_manual(values = my_colors) +
    scale_linetype_manual(values = c("solid", "dashed"))+
    scale_x_continuous(name = NULL, limits = c(0,336), expand = c(0,0) ,breaks = seq(0, 336, by = 24), 
                       labels = paste0("Day ", c(0:14)), minor_breaks = seq(0, 336, by = 12))+
    theme_minimal(base_size = 20)
}

comp_plot(temp_long_subset, 74, "Temperature (\u00B0F)")
comp_plot(hum_long_subset, 70, "Humidity (%)")
comp_plot(co2_long_subset, 2001, "Carbon Dioxide (ppm)")
comp_plot(pm2.5_long_subset, 55.5, expression("PM2.5 (" * mu * "g/m"^3 * ")"))
comp_plot(pm10_long_subset, 205, expression("PM10 (" * mu * "g/m"^3 * ")"))


hcho_long_subset <- long_subset_maker("HCHO", merged_df)
comp_plot(hcho_long_subset, 0.1, expression("Formalydhyde (mg/m"^3 * ")"))


co_long_subset <- long_subset_maker("Monoxide", merged_df)
comp_plot(co_long_subset, 0.9, "Carbon Monoxide (ppm)")


voc_long_subset <- long_subset_maker("Organic", merged_df)
comp_plot(voc_long_subset, 2001, "Volatile Organic Compounds (ppb)")
