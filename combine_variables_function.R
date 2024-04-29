#Zachary Delk
#Southface
#Creates a function that averages data across like columns
#Then returns a new data set

combine_variables = function(use_df){
#Creates a list of naming conventions to use later
test_list = c("Temp","Hum", "Dioxide","CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")
#Creates another list that is used for creating a new dataset
new_col_names = c("DateTime","Temp","Hum", "Dioxide", "CO2", "Organic", "PM2.5", "PM10", "HCHO", "Monoxide")

#Creates a matrix that has the number of rows of the orginal dataset and the coloums from the naming convention list
row_means_matrix <- matrix(NA, nrow = nrow(use_df), ncol = length(test_list))

#creates a new dataframe that only keeps the first coloumn of the orginal dataset. In this case the DateTime data
new_dataframe = use_df[, 1, drop = FALSE]

#For loop that iterates for each name in the naming convention list
for (i in 1:length(test_list)){
  #pulls the naming convention out of the list
  name = test_list[i]
  #Gets the row Mean of all columns that follow the current naming convention
  #Then fills the matrix
  row_means_matrix[,i] = rowMeans(dplyr::select(use_df,contains(name)), na.rm = TRUE)
}

#Binds the DateTime dataframe with the row matrix
new_dataframe = cbind(new_dataframe, row_means_matrix)
#Uses the other list above to rename all the columns in the new dataset
colnames(new_dataframe) = new_col_names

return(new_dataframe)
}

