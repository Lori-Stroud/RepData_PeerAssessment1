
---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
#Create file if file isn't downloaded to working director yet, this includes unzipping file 
  if(!file.exists("./activity")){dir.create("./activity")}
  
  if(!file.exists("activity.zip"))
  {
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile ="./activity.zip",method="auto")
    unzip("./activity.zip", "./")
  }else
  {
    unzip(zipfile = "./activity.zip",exdir = "./activity")
  }
  readPath <- file.path("./" , "activity")
  files<-list.files(readPath, recursive=TRUE)
  files
  activityData <- read.csv(file = "./activity/activity.csv", sep = ",", header = TRUE)

## What is mean total number of steps taken per day?
#Calculates the total, mean and median number of steps per day using aggreate function.
totalDailyActivityData <- aggregate(steps~date, data = activityData, sum, na.rm=TRUE)
totalDailyActivityData$meanSteps <- aggregate(steps~date, data = activityData, mean, na.rm=TRUE)[,"steps"]
totalDailyActivityData$medianSteps <- aggregate(steps~date, data = activityData, median, na.rm=TRUE)[,"steps"]

#Round mean and median steps since steps will always be a whole number and can't be a fraction.
totalDailyActivityData$meanSteps <- sapply(totalDailyActivityData$meanSteps, round, digits = 0)
totalDailyActivityData$meadianSteps <- sapply(totalDailyActivityData$medianSteps, round, digits = 0)

#Specify column names for better use later
names(totalDailyActivityData) = c("date","totalSteps","meanSteps","medianSteps")

totalDailyActivityData <- totalDailyActivityData[,c("date","totalSteps","meanSteps","medianSteps")]

#Table showing the total, mean and median number of steps for each day.
totalDailyActivityData

require(lattice)
require(lattice)
#Calculate the mean steps per 5-minute interval
meanIntervalActivityData <- aggregate(steps~interval, data = activityData, mean, na.rm=TRUE)
meanIntervalActivityData$steps <- sapply(meanIntervalActivityData$steps, round,digits = 0)

#Plot time series of mean steps per interval includes showing interval with max mean steps
xyplot(steps~interval, data = meanIntervalActivityData, type = 'l', xlab  = "Interval", ylab = "Average Steps", main="Average Steps Taken Per Interval",panel = function(x,y,...){
                panel.xyplot(x, y, ...)
                panel.abline(v=x[ which(y==max(meanIntervalActivityData$steps))], lty = 1, col = "black")
                panel.text(meanIntervalActivityData[meanIntervalActivityData$steps == max(meanIntervalActivityData$steps),"interval"],max(meanIntervalActivityData$steps),labels = paste("Mean MaxStep Interval = ",  meanIntervalActivityData[meanIntervalActivityData$steps == max(meanIntervalActivityData$steps),"interval"]))
            }, scales = list(x=list(rot=90)))

## What is the average daily activity pattern?
require(lattice)
#Calculate the mean steps per 5-minute interval
meanIntervalActivityData <- aggregate(steps~interval, data = activityData, mean, na.rm=TRUE)
meanIntervalActivityData$steps <- sapply(meanIntervalActivityData$steps, round,digits = 0)

#Plot time series of mean steps per interval includes showing interval with max mean steps
xyplot(steps~interval, data = meanIntervalActivityData, type = 'l', xlab  = "Interval", ylab = "Average Steps", main="Average Steps Taken Per Interval",panel = function(x,y,...){
                panel.xyplot(x, y, ...)
                panel.abline(v=x[ which(y==max(meanIntervalActivityData$steps))], lty = 1, col = "black")
                panel.text(meanIntervalActivityData[meanIntervalActivityData$steps == max(meanIntervalActivityData$steps),"interval"],max(meanIntervalActivityData$steps),labels = paste("Mean MaxStep Interval = ",  meanIntervalActivityData[meanIntervalActivityData$steps == max(meanIntervalActivityData$steps),"interval"]))
            }, scales = list(x=list(rot=90)))

# Weekday/Weekend classification of dates
imputedActivityData$weekType <- sapply(weekdays(as.Date(as.character(imputedActivityData$date))), function(x) if(x %in% c("Saturday","Sunday")) "weekend" else "weekday")

# Calcualtes steps mean per interval-weekdayclassification 
meanIntervalImputedActivityData <- aggregate(steps~interval+weekType, data = imputedActivityData, mean)
meanIntervalImputedActivityData$steps <- sapply(meanIntervalImputedActivityData$steps, round, digits = 0)

#Plot 1x2 time series of mean steps per interval including weekday clasiffications in each panel
xyplot(steps~interval|weekType, data = meanIntervalImputedActivityData, type = 'l', xlab  = "Interval", ylab = "Average Steps Taken", main="Average Steps Taken Per Interval For Imputed Daily Activity Data", scales = list(x=list(rot=90)), layout=c(1,2))