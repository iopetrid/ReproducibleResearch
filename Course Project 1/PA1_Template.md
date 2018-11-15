---
title: "Reproducible Research - Course Project 1"
date: "November 15, 2018"
author: "Ioannis Petridis"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Assignment Objectives
1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum    number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

### Load needed packages
```{r, echo=FALSE}
library(lubridate)
library(ggplot2)
library(dplyr)
library(zoo)
```
### Reading/Processing the dataset
```{r}
setwd("/Users/iopetrid/Desktop/Coursera/Data Science/5_Reproducible Research/Course Project 1")
raw_activity<-read.csv("./activity.csv")

#Get to know the data
head(raw_activity)
str(raw_activity)
summary(raw_activity)

activity<-raw_activity
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")


```
### What is mean total number of steps taken per day?
```{r}
# Caclulate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)

# Calculate and report the the mean and median of the total number of steps taken per day
mean_steps<-aggregate(steps ~ date, data = activity, mean)
median_steps<-aggregate(steps ~ date, data = activity, median)

# Make a histogram of the total number of steps taken per day
plot(steps_per_day,type="h",main = "Histogram of the total steps taken per day", xlab = "Steps", col = "blue")

```
### What is the average daily activity pattern?
```{r}
# Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

daily_aver<-aggregate(steps ~ interval, data = activity, FUN = mean)
plot(daily_aver, type = "l")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max(activity$steps, na.rm = TRUE)


```
### Imputing missing values
```{r}
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# NA -> mean
activity_no_nas <- activity
activity_no_nas$steps[is.na(activity_no_nas$steps)] <- mean(na.omit(activity$steps))


#Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity_no_nas$date <- as.Date(activity_no_nas$date, format = "%Y-%m-%d")

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
steps_per_day_no_NAs <- aggregate(steps ~ date, data = activity_no_nas, sum, na.rm = TRUE)
mean_steps_no_NAs<-aggregate(steps ~ date, data = activity_no_nas, mean)
median_steps_no_NAs<-aggregate(steps ~ date, data = activity_no_nas, median)

plot(steps_per_day,type="h",main = "With NAs", xlab = "Steps", col = "blue")
plot(steps_per_day_no_NAs,type="h",main = "Without NAs", xlab = "Steps", col = "red")

```
I cant say there is much difference from the original data

### Are there differences in activity patterns between weekdays and weekends?
```{r}
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity_no_nas$weekday <- factor(format(activity_no_nas$date, "%A"))
levels(activity_no_nas$weekday) <- list(weekday = c("Monday","Tuesday","Wednesday", "Thursday","Friday"), weekend=c("Saturday", "Sunday"))

#Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
par(mfrow = c(2, 1))

with(activity_no_nas[activity_no_nas$weekday == "weekend",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekends"))

with(activity_no_nas[activity_no_nas$weekday == "weekday",], plot(aggregate(steps ~ interval, FUN = mean), type = "l", main = "Weekdays"))
```

