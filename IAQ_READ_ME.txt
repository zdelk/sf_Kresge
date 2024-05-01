Zachary Delk for Southface
Proj: Kresage IAQ Data Analysis
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
This file is an accompaniment to multiple R code files, all related to Kresage IAQ data analysis. 
It will show my thought process and explain my reasoning for certain choices.
R code path: TBD
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!03.28.23
I made this file a while back, however it was messy and did not tell the whole story so I will make
an attempt to clean it up. I will section off each section by the correlated file name. I will do by best
to orginize the whole file as a step by step guide to the process I have started. This may change.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Removes_seconds.R
Reason for file: 
	The individual sensor data files come with 2 variables: sensor data and timestamp
	This time stamp includes seconds. Two sensors at the same locations will rairly have 
	the same exact time down to the seconds so having this makes it hard to join the 
	datasets. To get around this we will remove the seconds from the time stamp and then
	send it to SQL. (I would like to find a way to add them in R so I dont have to go to SQL)
	
File Process:
	Since we want to remove seconds from alot of seperate files we use a for loop that references
	the total number of files in a set folder. It will go into that folder using the path specified 
	and pull all the files that match a set criteria. It will then preform the process of converting the 
	timestamp data to month/day/hour/minute thus removing the seconds. It will the write this new dataframe
	to a new file that follows the naming convention new-{OG filename}.csv 
	These new files will appear in the original folder. We will keep the old files just in case.
	
Extra:
	I have been having some issues with converting the datetime data. Sometimes it likes 
	read_csv sometimes it likes read.csv. Have not figured out a work around as of yet.
	Also depending on how you can edit data as you pull it from the sensor site, this code
	might be pointless. But until then it is usefull
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Removes_empty_columns.R
Reason for file:
	While this is not extremly common, there have been instances where we will have completly empty columns in the 
	mass data sets that inclueds all the sensors. They make the files bigger than they need to be and therefore we 
	need to remove them.

File Process:
	The process is similar to Removes_seconds.R. We will set the folder and then have the loop repeat this process
	for all files in a given folder. At the very top of the code file there is a test that works on a single file
	that you can specify. The new files the loop outputs have the same naming convention as Removes_seconds.
	Since the files this code works on is aggregated data, it will have a different name then the individual sensor
	datfiles so we dont run into any naming problems.
	
Extra:
	It seems like I shouldn't need this code. I feel like there is a way to specify this when you intially pull data
	from the sensor site.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!test_site.R 
Reason for file:
	This is a code file I have been using as a test site before moving it to the main branch which I will talk about
	next. It will not be commented as much the main branch. I reserve the right to use it a scratch work before setting
	anything in stone on main.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Cleaning_analysis.R 
Reason for file:
	This is where the actual data analysis happens. The thought process is once data has been aggregated and had the 
	basic cleaning done. We will run it throught this code for a one-stop-shop for all the analystics we need.
	
File Process:
	Intro:
	I am trying my best to heavily comment the code itself. Thus this section will give an overview as to the processes
	I chose to go with. 
	Starting of we just load in all the libraries we will use. I like to front load this so I can get it out of the way
	and don't have to go digging in the code to verify. There is always a possibility that I will need to add another library
	or can remove another. I will try to keep an updated list below with a brief explanation of my reasoning. 
	After libraries are in we will load in the data set. For sanity I have it were you will change the assigned name
	of the intial dataframe depending on the datafile you are looking at. Immediately after that we will subset the dataframe
	to remove extra DateTime columns and give it the name (df).
	The next section works on adding days of the week, time of day, and averages for data type by row.
	Just general formatting that will be useful later.
	
	Analysis:
	Here I have basic aggregations for types of sensor data (e.g. Co2, Temp). Once we get
	results back from the completed builds I will be able to add actually hypothesis testing but as for now thats all I
	can do.
	
	Vizulization:
	This section revolves around all things vizualization. For now I have basic boxplots and a scatterplot with trendline.
	One thing I want to look in to is autopropigating the chart name with the name of the data file. One make it so there is one less thing to change when doing reports.
	
Libraries:
	readr - easy way to imprt .csv files allows for some customization
		
	ggplot2 - as far as I know, it's the gold standard for plotting in R. Will use it for all vizulizations that will
		go on reports
			
	lubridate- only thing I use it for is to convert dates
		
	dplyr - does some extra data manipulation. Also handles dataframes very well
		
	openair - New to me. used for air currents. But can also do very good time averages and you can specify the interval
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Stuff I had before and don't want to remove from this file:
	
Step-by-Step Data Cleaning for IAQ Data
1. Convert data files to csv files
2. Load files into SQL
3. LEFT JOIN all relevant files ON DATETIME
4. Export file as .csv NAMING CONVENTION = {city}_Full_IAQ_Data.csv
5. Load full data in R. Use read_csv (read.csv converts dates to character)
6. Subset full data set to remove 

-Look for a way to removed extra datetime columns with 
	something similar to "LIKE" statement
	
	
Albany#
Atlanta - Peachtree Tower - all in single files
Atlanta - East Lake#
Conyers
Griffin
LaGrange
Milledgeville#

