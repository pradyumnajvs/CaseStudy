library(ggplot2)
library(plyr)

rm(list = ls())
setwd("C:/Users/vj853t/Documents/DataScience/Case Study/Case Study 2")
crime_data <- read.csv("~/DataScience/Case Study/Case Study 2/CrimeData2.csv")

names(crime_data)
str(crime_data)

#1. Overall trend in crimes for the whole period of time in the dataset. 
#The granularity should be at the Day level.
crime_data$DateTime <- as.POSIXct(crime_data$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz="EST")

head(crime_data$DateTime)
str(crime_data)
crime_data$Date <- as.Date(crime_data$DateTime, tz="EST")
str(crime_data)

crimecount_bydate <- aggregate(crime_data$Date, list(date = crime_data$Date), FUN=length)
str(crimecount_bydate)
colnames(crimecount_bydate) <- c('Date', 'CrimeCount')

ggplot(crimecount_bydate, aes(Date, CrimeCount, color = Date)) + geom_line() + ggtitle('Crime Count Distribution by Day')

#2. Which are the most and the least dangerous hours in Philadelphia?
crime_data$Hour <- strftime(crime_data$DateTime, format = '%H', tz='EST')
str(crime_data)

crimecount_byhour <- aggregate(crime_data$Hour, list(Hour = crime_data$Hour), FUN=length)
str(crimecount_byhour)
colnames(crimecount_byhour) <- c('Hour', 'CrimeCount')

crimecount_byhour$Hour <- as.integer(crimecount_byhour$Hour)

ggplot(crimecount_byhour, aes(Hour, CrimeCount)) + geom_line(color = 'Red') + ggtitle('Crime Count Distribution by Hour')

#3. Is there any seasonality in the crime rate?
crime_data$Month <- strftime(crime_data$DateTime, format = '%m', tz='EST')
str(crime_data)

crimecount_bymonth <- aggregate(crime_data$Month, list(Month = crime_data$Month), FUN=length)
str(crimecount_bymonth)
colnames(crimecount_bymonth) <- c('Month', 'CrimeCount')
str(crimecount_bymonth)

ggplot(crimecount_bymonth, aes(Month, CrimeCount)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Month')

#4. What are the top 10 crimes crime types?
top_crimetypes <- aggregate(crime_data$Text_General_Code, list(Type = crime_data$Text_General_Code), FUN = length)
colnames(top_crimetypes) <- c('CrimeType', 'Count')
str(top_crimetypes)
top_crimetypes <- head(arrange(top_crimetypes,desc(Count)), n=33)

ggplot(top_crimetypes, aes(CrimeType, Count)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Crime Type')

#5. Which police HQ is in the most need of strengthening?
crimecount_bypoliceHQ <- aggregate(crime_data$Dc_Dist, list(Type = crime_data$Dc_Dist), FUN = length)
colnames(crimecount_bypoliceHQ) <- c('District', 'CountOfCrime')
str(crimecount_bypoliceHQ)
head(arrange(crimecount_bypoliceHQ, desc(CountOfCrime)), n=10)
