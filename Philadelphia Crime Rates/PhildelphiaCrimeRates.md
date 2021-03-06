---
title: "Philadelphia Crime Rates Dataset Exploration"
author: "Pradyumna Janga"
date: "August 21, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

This is an R Markdown document for Philadelphia Crime Rate Dataset. 
```{r echo=FALSE, message=FALSE, warning=FALSE, message=FALSE, warning=FALSE, packages}
# Load all of the packages that you end up using
# in your analysis in this code chunk.
library(ggplot2)
library(plyr)
```

Let us first load the data and check the structure of the dataset
```{r echo=FALSE, message=FALSE, warning=FALSE, Load_the_Data}
# Load the Data
rm(list = ls())
setwd("C:/Users/username/Documents/DataScience/Case Study/Case Study 2")
crime_data <- read.csv("~/DataScience/Case Study/Case Study 2/CrimeData2.csv")
str(crime_data)
names(crime_data)
```

There are 3 variables and 210000 observations in the dataset.

It can be observed that the DISPATCH DATE TIME column is stored as a factor datatype instead of DateTime datatype.Let us convert that column to a proper format and attach a timezone so that we can work on it easily. Instead of replacing the entire DISPATCH_DATE_TIME column, let us create a new column.

```{r echo=FALSE, crimetimezoneset}
crime_data$DateTime <- as.POSIXct(crime_data$Dispatch_Date_Time, format = "%Y-%m-%d %H:%M:%S", tz="EST")
head(crime_data$DateTime)
str(crime_data)
```
In order to calculate the crimes per day, we need a new column which has only dates without time

```{r echo=FALSE, crimedate}
crime_data$Date <- as.Date(crime_data$DateTime, tz="EST")
str(crime_data)
```

## Overall trend in crimes for the whole period of time in the dataset (The granularity should be at the Day level)
Now we need to calculate the number of crimes that occur in Philadephia per day. 
```{r echo=FALSE, crime_trend}
crimecount_bydate <- aggregate(crime_data$Date, list(date = crime_data$Date), FUN=length)
str(crimecount_bydate)
colnames(crimecount_bydate) <- c('Date', 'CrimeCount')
```

The dataset has data from January 1st 2006 until December 31st 2015. The distrubution of crime per day per year can be observed from the line plot  
```{r echo=FALSE, crime_trend_plot}
ggplot(crimecount_bydate, aes(Date, CrimeCount, color = Date)) + geom_line() + ggtitle('Crime Count Distribution by Day')
```
Overall the number of crimes from 2006 until 2016 beginning has been reduced. It can also be seen that every year begins with lower rate of crimes which go up during the middle of the year and reduce by the end of the year.

## Which are the most and the least dangerous hours in Philadelphia?
Even though we know the overall trend per year, let us observe which hours in a day are the most dangerous and which hours are the least dangerous.
```{r echo=FALSE, crime_trend_day}
crime_data$Hour <- strftime(crime_data$DateTime, format = '%H', tz='EST')
str(crime_data)
```

```{r echo=FALSE, crime_day_trend}
crimecount_byhour <- aggregate(crime_data$Hour, list(Hour = crime_data$Hour), FUN=length)
str(crimecount_byhour)
colnames(crimecount_byhour) <- c('Hour', 'CrimeCount')
crimecount_byhour$Hour <- as.integer(crimecount_byhour$Hour)
```

The distrubution of number of crimes per hour in a day can be observed from the line plot  
```{r echo=FALSE, crime_day_trend_plot}
ggplot(crimecount_byhour, aes(Hour, CrimeCount)) + geom_line(color = 'Red') + ggtitle('Crime Count Distribution by Hour')
```

The number of crimes drop starting from midnight until sunrise when they start to increase. At around 3:00 PM to 5:00 PM in the afternoon the number hits the peak and almost as study until the midnight before they fall drastically.

## Is there any seasonality in the crime rate?
In order to analyze if there is any seasonality in the crime rate, one needs to aggregate the number of crimes per month. Before that can be done, DateTime column has to be converted from character to Date-time function.
```{r, echo=FALSE, crime_seasonality}
crime_data$Month <- strftime(crime_data$DateTime, format = '%m', tz='EST')
str(crime_data)
```

Now that a new column with date time format has been created, let us create a new variable with crimes aggregated per month
```{r, echo=FALSE, crime_seasonality_month}
crimecount_bymonth <- aggregate(crime_data$Month, list(Month = crime_data$Month), FUN=length)
str(crimecount_bymonth)
colnames(crimecount_bymonth) <- c('Month', 'CrimeCount')
str(crimecount_bymonth)
```

Let us plot the average number of crimes per month
```{r, echo=FALSE, crime_seasonality_month_plot}
ggplot(crimecount_bymonth, aes(Month, CrimeCount)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Month')
```

It can be inferred from the plot that the most crimes occur between May to August (which is during the Summer). The  number of crimes tend to go low starting from Fall and hit all time low during the Winter (December to Febuary)

## What are the top 10 crimes crime types?
In all the above cases, we tried aggregating the number of crimes per day, per hour and per month. This time we will aggregate the number of crimes per crime type instead of any dateTime based variables
```{r echo=FALSE, crime_toptypes}
top_crimetypes <- aggregate(crime_data$Text_General_Code, list(Type = crime_data$Text_General_Code), FUN = length)
colnames(top_crimetypes) <- c('CrimeType', 'Count')
str(top_crimetypes)
top_crimetypes <- head(arrange(top_crimetypes,desc(Count)), n=10)
top_crimetypes
```

```{r, echo=FALSE, Crime_toptypes_plot}
ggplot(top_crimetypes, aes(CrimeType, Count)) + geom_bar(fill = 'Blue', stat = "identity") + ggtitle('Crime Count Distribution by Crime Type')
```

## Which police HQ is in the most need of strengthening?
In order to analyze which police HQ needs strengthening, we need to calculate the number of crimes per District Police HQ. We will aggregate the number off crimes per District.
```{r, echo=FALSE,  crime_policeHQ}
crimecount_bypoliceHQ <- aggregate(crime_data$Dc_Dist, list(Type = crime_data$Dc_Dist), FUN = length)
colnames(crimecount_bypoliceHQ) <- c('District', 'CountOfCrime')
str(crimecount_bypoliceHQ)
head(arrange(crimecount_bypoliceHQ, desc(CountOfCrime)), n=10)
```
From this data, we can conclude that Police District HQ 15 needs more strengthening of police.
