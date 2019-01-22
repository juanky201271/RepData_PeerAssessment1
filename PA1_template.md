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
plot(Datos.sin.na$date, Datos.sin.na$steps, type = "h", main = "Total number of steps taken each day", xlab = "Date", ylab = "Steps")
```

![](PA1_template_files/figure-html/histWna-1.png)<!-- -->

### 2. Calculate and report the mean and median total number of steps taken per day


```r
T.mean <- Datos.sin.na %>% group_by(date) %>% summarize_at(c("steps"), mean) %>% rename(T.mean = steps)
T.median <- Datos.sin.na %>% group_by(date) %>% summarize_at(c("steps"), median) %>% rename(T.median = steps)
T.mean.median <- merge(T.mean, T.median)
T.mean.median
```

```
##          date     T.mean T.median
## 1  2012-10-02  0.4375000        0
## 2  2012-10-03 39.4166667        0
## 3  2012-10-04 42.0694444        0
## 4  2012-10-05 46.1597222        0
## 5  2012-10-06 53.5416667        0
## 6  2012-10-07 38.2465278        0
## 7  2012-10-09 44.4826389        0
## 8  2012-10-10 34.3750000        0
## 9  2012-10-11 35.7777778        0
## 10 2012-10-12 60.3541667        0
## 11 2012-10-13 43.1458333        0
## 12 2012-10-14 52.4236111        0
## 13 2012-10-15 35.2048611        0
## 14 2012-10-16 52.3750000        0
## 15 2012-10-17 46.7083333        0
## 16 2012-10-18 34.9166667        0
## 17 2012-10-19 41.0729167        0
## 18 2012-10-20 36.0937500        0
## 19 2012-10-21 30.6284722        0
## 20 2012-10-22 46.7361111        0
## 21 2012-10-23 30.9652778        0
## 22 2012-10-24 29.0104167        0
## 23 2012-10-25  8.6527778        0
## 24 2012-10-26 23.5347222        0
## 25 2012-10-27 35.1354167        0
## 26 2012-10-28 39.7847222        0
## 27 2012-10-29 17.4236111        0
## 28 2012-10-30 34.0937500        0
## 29 2012-10-31 53.5208333        0
## 30 2012-11-02 36.8055556        0
## 31 2012-11-03 36.7048611        0
## 32 2012-11-05 36.2465278        0
## 33 2012-11-06 28.9375000        0
## 34 2012-11-07 44.7326389        0
## 35 2012-11-08 11.1770833        0
## 36 2012-11-11 43.7777778        0
## 37 2012-11-12 37.3784722        0
## 38 2012-11-13 25.4722222        0
## 39 2012-11-15  0.1423611        0
## 40 2012-11-16 18.8923611        0
## 41 2012-11-17 49.7881944        0
## 42 2012-11-18 52.4652778        0
## 43 2012-11-19 30.6979167        0
## 44 2012-11-20 15.5277778        0
## 45 2012-11-21 44.3993056        0
## 46 2012-11-22 70.9270833        0
## 47 2012-11-23 73.5902778        0
## 48 2012-11-24 50.2708333        0
## 49 2012-11-25 41.0902778        0
## 50 2012-11-26 38.7569444        0
## 51 2012-11-27 47.3819444        0
## 52 2012-11-28 35.3576389        0
## 53 2012-11-29 24.4687500        0
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
plot(Datos.imp.i$date, Datos.imp.i$steps, type = "h", main = "Total number of steps taken each day - impute NA", xlab = "Date", ylab = "Steps")
```

![](PA1_template_files/figure-html/histImpute-1.png)<!-- -->


```r
T.mean.imp.i <- Datos.imp.i %>% group_by(date) %>% summarize_at(c("steps"), mean) %>% rename(T.mean = steps)
T.median.imp.i <- Datos.imp.i %>% group_by(date) %>% summarize_at(c("steps"), median) %>% rename(T.median = steps)
T.mean.median.imp.i <- merge(T.mean.imp.i, T.median.imp.i)
T.mean.median.imp.i
```

```
##          date     T.mean T.median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```

### Do these values differ from the estimates from the first part of the assignment? 
### **Of course yes, A constant value appears in the days that only have NA's**

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### **This is my opinion: I can not use the variable ' date ', because all the values of certain days are ' NA ', so I can not impute any value. Using the variable ' interval ' is possible to impute values with the average per interval. With this the average and medium of these days are totally distorted. Maybe the best option is to impute all the NA with zeros, but of course, I'm not sure, I'm learning...**

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
