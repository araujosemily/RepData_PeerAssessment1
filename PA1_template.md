---
title: '*Reproducible Research: Peer Assessment 1*'
author: "Emily"
date: "13/02/2020"
output: html_document
---




# **Introduction**

  The results of the first peer assessments of the course Reproducible Research of  coursera are presented in this document. The data used in this assessments comes from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. 

  During the months of October and November of 2012, the data was collect from from an anonymous individual, including include the number of steps taken in 5 minute intervals each day. 
  
  This document presents the answers for the Reproducible Research's Peer Assessment 1 using a single R markdown file that can be processed by knitr and be transformed into an HTML file. The analysis of the data showed that, during the week, the individual performs less physical activities, while this increases during the weekend. It is possible that the activity pattern observed in this study reflects the work routine of the individual. Additionally, it is possible to infer that, during the weekends, the analysed individual would have more time to practice physical activities than during the weekdays.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

rm(list = ls())
getwd()

setwd("C:/Users/hp/Documents/R 2019/data_science_specialization/Reproducible research - Project 1/Dataset")



```

```{r}

## Loading the Data
library(ggplot2)
library(plyr)


activity <- read.csv("activity.csv")


## Processing the Data

activity$day <- weekdays(as.Date(activity$date))


activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
  
```

## **Q1 -  What is mean total number of steps taken per day?**

### *For this part of the assignment, you can ignore the missing values in the dataset.*
```{r, echo=TRUE}
## pulling data without NA

clean <- activity[!is.na(activity$steps),]
```

### *Calculate the total number of steps taken per day*

### *summarizing total steps per date*

```{r}
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )

colnames(sumTable)<- c("Date", "Steps")
```

### **Make a histogram of the total number of steps taken each day**

### *Creating the historgram of total steps per day*
```{r, echo=TRUE}

hist(sumTable$Steps,breaks=5, xlab="Steps", main = "Total Steps per Day", col = "blue")

rug(sumTable$Steps)

```

## **Calculate and report the mean and median of the total number of steps taken per day**
```{r}
## Mean of Steps
as.integer(mean(sumTable$Steps))

## Median of Steps
as.integer(median(sumTable$Steps))

```

### **What is the average daily activity pattern?**

### *Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```{r, echo=TRUE}
StepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

plot(as.numeric(names(StepsPerInterval)), 
     StepsPerInterval, 
     xlab = "Interval", 
     ylab = "Steps", 
     main = "Average Daily Activity Pattern", 
     type = "l")
```

### **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**
 
      maxInterval <- names(sort(StepsPerInterval, decreasing = TRUE)[1])
      maxInterval
   
      maxSteps <- sort(StepsPerInterval, decreasing = TRUE)[1]
      maxSteps
     
```{r}
      #Imputing missing values
      
      # 1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
      ## Number of NAs in original data set
      nrow(activity[is.na(activity$steps),])
      
```

### **2 - Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**
     
### *The strategy for filling in NAs will be to substitute the missing steps with the average 5-minute interval based on the day of the week.*
```{r}      
      library(plyr)
      
      ## Create the average number of steps per weekday and interval
      StepsPerInterval <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
      
      ## Create dataset with all NAs for substitution
      nadata<- activity[is.na(activity$steps),]
      
      ## Merge NA data with average weekday interval for substitution
      newdata<-merge(nadata, StepsPerInterval, by=c("interval", "day"))
      #View(newdata)      
      
```
### **3- Create a new dataset that is equal to the original dataset but with the missing data filled in.**

### *Reorder the new substituded data in the same format as clean data set*
```{r}

      newdata2<- newdata[,c(6,4,1,2,5)]
      
      colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")

## Merge the NA averages and non NA data together
      mergeData <- rbind(clean, newdata2)
```
      

### **4-  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**
```{r}    
       ## Create sum of steps per date to compare with step 1
      
        sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
        
        colnames(sumTable2)<- c("Date", "Steps")
      
       ## Mean of Steps with NA data taken care of
        as.integer(mean(sumTable2$Steps))
        
        ## *Median of Steps with NA data taken care of
        as.integer(median(sumTable2$Steps))
```

      
### **Creating the histogram of total steps per day, categorized by data set to show impact**
```{r, echo=TRUE}     
        hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="red")
        
        hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="blue", add=T)
        
        legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("red", "blue") )
```

### **Are there differences in activity patterns between weekdays and weekends?**
        
### *Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day*.

```{r,echo=TRUE}      
        ## 1 - Create new category based on the days of the week
        mergeData$DayCategory <- ifelse(mergeData$day %in% c("sábado", "domingo" ), "Weekend", "Weekday")
```       
        
### **2 - Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)**.
```{r,echo=TRUE}       
        library(lattice) 
        ## Summarize data by interval and type of day
        intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))
        
        
        xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",  layout = c(1,2),
               main="Average Steps per Interval Based on Type of Day", 
               ylab="Average Number of Steps", xlab="Interval")

```





