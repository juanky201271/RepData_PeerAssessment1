---
title: "Reproducible Research: Peer Assessment 1: JCCC"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
### Show any code that is needed to
### 1. Load the data


```r
library(lattice)
library(plyr)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Datos <- read.csv("./activity/activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
Datos$date <- as.Date(Datos$date, "%Y-%m-%d")
Datos.sin.na <- Datos[!is.na(Datos$steps), ]
```

## What is mean total number of steps taken per day?
### For this part of the assignment, you can ignore the missing values in the dataset
### 1. Make a histogram of the total number of steps taken each day


```r
steps.date.sin.na <- Datos %>% group_by(date) %>% summarize_at(c("steps"), sum, na.rm = TRUE) %>% rename(T.sum = steps)
hist(steps.date.sin.na$T.sum, main = "Total number of steps taken each day", xlab = "Steps")
```

![](PA1_template_files/figure-html/histWna-1.png)<!-- -->

### 2. Calculate and report the mean and median total number of steps taken per day


```r
T.mean <- steps.date.sin.na %>% summarize_at(c("T.sum"), mean) %>% rename(T.mean = T.sum)
T.median <- steps.date.sin.na %>% summarize_at(c("T.sum"), median) %>% rename(T.median = T.sum)
T.mean.median <- merge(T.mean, T.median)
T.mean.median
```

```
##    T.mean T.median
## 1 9354.23    10395
```

## What is the average daily activity pattern?
### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
DT.mean <- Datos.sin.na %>% group_by(interval) %>% summarize_at(c("steps"), mean) %>% rename(Mean.steps = steps)
plot(DT.mean$interval, DT.mean$Mean.steps, type = "l", main = "The mean of steps (all days) taken each 5-minute interval", xlab = "5-minute interval", ylab = "Mean steps (all days)")
```

![](PA1_template_files/figure-html/plotTS-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
DT.mean$interval[DT.mean$Mean.steps == max(DT.mean$Mean.steps)]
```

```
## [1] 835
```

## Imputing missing values
### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(Datos[is.na(Datos$steps), ])
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or **the mean for that 5-minute interval**, etc. 
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
Datos.imp.i <- Datos
for(i in 1:length(Datos.imp.i$steps)) {
  if (is.na(Datos.imp.i$steps[i])) {
    imp.mean <- Datos.imp.i$steps[Datos.imp.i$interval == Datos.imp.i$interval[i] & !is.na(Datos.imp.i$steps)]
    Datos.imp.i$steps[i] <- mean(imp.mean)  
  }
}
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.


```r
steps.date.imp.na <- Datos.imp.i %>% group_by(date) %>% summarize_at(c("steps"), sum) %>% rename(T.sum = steps)
hist(steps.date.imp.na$T.sum, main = "Total number of steps taken each day - impute NA", xlab = "Steps")
```

![](PA1_template_files/figure-html/histImpute-1.png)<!-- -->


```r
T.mean.imp.i <- steps.date.imp.na %>% summarize_at(c("T.sum"), mean) %>% rename(T.mean = T.sum)
T.median.imp.i <- steps.date.imp.na %>% summarize_at(c("T.sum"), median) %>% rename(T.median = T.sum)
T.mean.median.imp.i <- merge(T.mean.imp.i, T.median.imp.i)
T.mean.median.imp.i
```

```
##     T.mean T.median
## 1 10766.19 10766.19
```

### Do these values differ from the estimates from the first part of the assignment? 
### **Of course yes, A constant value appears in the days that only have NA's**

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### **This is my opinion: I can not use the variable 'date', because all the values of these days are 'NA', so I can not impute any value. Using the variable 'interval' is possible to impute values with the mean per interval. With this, the mean and median of these days are totally distorted. Maybe the best option is to impute all the NA with zeros, but of course, I'm not sure, I'm learning...**

## Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part
### 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
Datos.imp.i$wf <- weekdays(Datos.imp.i$date, abbreviate = TRUE)
Datos.imp.i$wf[Datos.imp.i$wf %in% c("sá.","do.")] <- "weekend"
Datos.imp.i$wf[Datos.imp.i$wf != "weekend"] <- "weekday"
Datos.imp.i$wf <- factor(Datos.imp.i$wf)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
DTimp.mean <- Datos.imp.i %>% group_by(interval, wf) %>% summarize_at(c("steps"), mean) %>% rename(Mean.steps = steps)
xyplot(DTimp.mean$Mean.steps ~ DTimp.mean$interval | DTimp.mean$wf, type = "l", layout = c(1,2),  main = "The mean of steps (all days) taken each 5-minute interval", xlab = "5-minute interval", ylab = "Mean steps (all days)")
```

![](PA1_template_files/figure-html/xyplot-1.png)<!-- -->
