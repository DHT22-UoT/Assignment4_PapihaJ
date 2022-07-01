#Class: Special Topics
#Name: Papiha Joharapurkar
#Date: June 28 2022
#Assignment: MARTIANS ARE COMING!

# Read the data into a dataframe (check to make sure that column names do not have spaces in them)
# Loading required packages 
library(lubridate)
library(dplyr)

#Setting working directory and reading in the data into a dataframe, named martians_df
setwd("/Users/papihajoharapurkar/Documents/Masters/")
martians_df <- read.csv("ufo_subset.csv") #26008

#Renaming column names so they do not contain spaces/periods, instead replaced with _
martians_df = rename(martians_df, c('duration_seconds' = 'duration..seconds.'))
martians_df = rename(martians_df, c('duration_hours_mins' = 'duration..hours.min.'))
# Comment - Alternatively you could have done the following in one line using the pipe function:
  # final <- ufo_subset %>%
  # rename(duration.seconds = duration..seconds., duration.hours.min = duration..hours.min.)

#Clean up the rows that do not have Country or Shape information
  # Comment - I believe this could be achieved by filtering out the rows which have missing values in the Country and Shape column
  # Comment - you could try
      # filter(!is.na(country)) %>%
      # filter(!is.na(shape)) %>%
#Converting state category into factor instead of keeping the character-type
martians_df$state <- as.factor(martians_df$state)
# Comment - it's unclear why you did this step, I don't think this was asked in the assignment

#Column called duration_time, replaces duration_hours_mins column with seconds_to_period function
martians_df$duration_hours_mins <- seconds_to_period(martians_df$duration_seconds)
# Comment - it's unclear why you did this step, I don't think this was asked in the assignment

#Converted Datetime and Date_posted columns into appropriate formats with as.POSIXct function
martians_df$date.posted <- as.POSIXct(martians_df$date.posted, format = "%Y-%m-%d")
martians_df$datetime <- as.POSIXct(martians_df$datetime, format = "%Y-%m-%d %H:%M:%S")

# NUFORC officials comment on sightings that may be hoax.
# Figure out a way (go through the Comments and decide how a proper filter should look like)
# and remove these sightings from the dataset.
# Used grepl function to locate rows containing "hoax" and removed them from the dataframe 
martians_df <- martians_df[!grepl("hoax", martians_df$comments, ignore.case = T), ] #Removed 245 sightings - at 25763
# Comment - alternative: filter(!grepl("\\b[HOAX]", comments, ignore.case=T))

# Add another column to the dataset (report_delay) and populate with the time difference in days,
# between the date of the sighting and the date it was reported.
# Used seconds_to_period function to convert difference in time-periods into seconds and extracted day value 
martians_df$report_delay <- martians_df$date.posted - martians_df$datetime
martians_df$report_delay <- seconds_to_period(martians_df$report_delay)$day
# Comment - I can see that this makes sense because of the way you converted the format of the datetime and date.posted columns
# Comment - alternative: convert datetime column to a format without the time included 
  # Comment - mutate(datetime = gsub(" .*", "", datetime))
# Comment - and then subtract one column from the other which gives the result in days
  # Comment - mutate(report_delay = as.Date(date.posted) - as.Date(datetime))

# Filter out the rows where the sighting was reported before it happened.
# Kept days with report_delay = 0 in case report occurred on same day as sighting
martians_df <- martians_df %>% filter(report_delay >= 0) #removed 7 sightings - at 25756 

# Create a table with the average report_delay per country.
martians_df$country[martians_df$country == ""] <- NA
martians_df <- martians_df %>% filter(!is.na(martians_df$country))
table_average_report_delay <- martians_df %>% group_by(country) %>% summarise(Average_Report_Delay = mean(report_delay, na.rm=T))
table_average_report_delay
# Comment - alternative: add to existing pipe function
  # filter(!is.na(country)) %>%
  # mutate(report_delay = as.Date(date.posted) - as.Date(datetime)) %>%
  # filter(!datetime > date.posted) %>%
  # group_by(country) %>%
  # summarise(Avg_Report_Delay = round(mean(report_delay), 2))

# Check the data quality (missingness, format, range etc) of the duration(seconds) column.
# Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.

#The following functions verify there is no data missing or any NA values 
which(is.na(martians_df$duration_seconds))
martians_df[martians_df$duration_seconds == "",]

#With summary function, can see are sightings that hold a wide-time duration of observation, spanning from 0 to 82800000
#Median is smaller than mean, infers positively-skewed distribution i.e. greater number of observations that have low values 
#To deal with this data, a log-transformation can be performed, involves taking natural log of data values 
#Outliers will also be removed, these are observations that are below Q1 - 1.5*IQR or above Q3 + 1.5*IQR
martians_df$duration_seconds %>% summary() 

IQR_df <- IQR(martians_df$duration_seconds)
quantile_1 <- as.numeric(quantile(martians_df$duration_seconds)[2])
quantile_3 <- as.numeric(quantile(martians_df$duration_seconds)[4])
lower_limit <- (quantile_1 - 1.5*IQR_df)
upper_limit <- (quantile_3 + 1.5*IQR_df)
martians_df <- martians_df %>% filter(duration_seconds >= lower_limit) %>% filter(duration_seconds <= upper_limit)

#The format of the duration_seconds is numeric, however observations indicate that initial observations are smaller < 1 and with decimal values 
#The larger values,  seen by the max_values object are larger, without any decimal values 
str(martians_df$duration_seconds)
min_values <- martians_df %>% arrange(duration_seconds) %>% select(duration_seconds) %>% head()
min_values
max_values <- martians_df %>% arrange(-duration_seconds) %>% select(duration_seconds) %>% head()
max_values

# Create a histogram using the duration(seconds) column.
#Since the data is positively-skewed, a log-transformation of the values was performed so the distribution tends to a normal distribution 
hist(log(martians_df$duration_seconds), xlab="Log(duration(seconds))", ylab ="Frequency of Observations", main="Histogram of Duration (seconds)", xlim=c(0,8))

