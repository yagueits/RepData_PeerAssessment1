

```r
---
title: 'REPRODUCIBLE RESEARCH. Peer-graded Assignment: Course Project 1'
author: "Santiago Peña"
date: "25 de diciembre de 2017"
# output: html_document
keep_md: true 
---


I want to apologize in advance for my lack of command in english. I tried to do my best. Thank you in advance for you empathy. This report require the installation of the packages ggplot2, lubridate and chron. (install.packages("package")). 

```

```
## Error: <text>:10:3: unexpected symbol
## 9: 
## 10: I want
##       ^
```


# Introduction

The goal of this report is to analyze and study the data contained in the Activity Monitoring, trying to:

 * Loading and preprocessing the data
 * Calculate the mean total number of steps taken per day
 * Calculate the average daily activity pattern
 * Imputing missing values
 * Assess any differences in activity patterns between weekdays and weekends


# Loading and preprocessing the data

The data is directly downloaded from the url https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip and saved into the "Data" dataframe as follows:


```r
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
```

To check if there is any need of additional transformation or processing we verify the structure of the data


```r
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

It shows the dates treated as factors so it will be changed to a date format as follows:


```r
data$date<-ymd(data$date)
```

# Calculate the mean total number of steps taken per day


```r
numTotalPasos<-sum(data$steps, na.rm=T)
```


El número total de pasos por día es de 570608.

The following plot represents the histogram of the total number of steps taken each day. 


```r
data2<-aggregate(steps ~ date, data, sum)
ggplot(data2, aes(steps))+geom_histogram(na.rm = T, fill="blue")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
MeanStepsPerDay<-mean(data2$steps, na.rm = T)
MedianStepsPerDay<-median(data2$steps, na.rm = T)
```
And the mean and median of the steps per day are 1.0766189 &times; 10<sup>4</sup> and 10765. 

# Calculate the average daily activity pattern

To plot the average daily pattern firstly we create a new dataframe 



```r
data3 <- aggregate(steps ~ interval, data, FUN = mean, na.rm = TRUE)
ggplot(data3, aes(interval, steps))+geom_line()+xlab("Five minutes interval")+ylab("Average number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

```r
maxAveSteps<-data3[which.max(data3$steps),]
```

Being the maximum value, as it can be seen in the plot 206.1698113, in the interval 835

# Imputing missing values

This section should include:

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
numNas<-sum(is.na(data$steps))
```
The number of NA values included is 2304 

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. and create a new dataset that is equal to the original dataset but with the missing data filled in.

As suggested ,it has been assumed the mean value per interval as the criteria to fill the NA's values. 


```r
data4<-data
for (i in 1:nrow(data4)) {
  if (is.na(data4[i,1])) {
    data4[i,1] <- data3[data3$interval == data4[i,3],2]
  }
}
data5<-aggregate(steps ~ date, data4, sum)
```

With this criteria, the histogram of 


```r
ggplot(data5, aes(steps,fill = "red")) + 
       geom_histogram() + 
        labs(title="Histogram of Steps per Day")+
        xlab("Steps per Day")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
mediasteps<-mean(data5$steps)
mediansteps<-median(data5$steps)
```

And the mean and median are 1.0766189 &times; 10<sup>4</sup> and 1.0766189 &times; 10<sup>4</sup>, hence the mean is the same as the previously obtained and there is a slight difference in the median. The impact of imputing missing data on the estimates of the total daily number of steps is negligible.


# Assess any differences in activity patterns between weekdays and weekends

The differences between the weekends and weekday data can be shown in the plot below. 


```r
data4$weekend <- ifelse(is.weekend(data4$date), "weekend", "weekday")
data6 <- aggregate(steps ~ interval + weekend, data4, mean)

ggplot(data6, aes(interval,steps))+geom_line()+facet_grid(.~weekend)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
```

