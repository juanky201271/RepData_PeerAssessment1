---
title: "Reproducible Research: Peer Assessment 1: JCCC"
output: 
  html_document:
    keep_md: true
---




```r
library(lattice)
library(plyr)
library(dplyr)
```

## Loading and preprocessing the data
### 1. Load the data


```r
Datos <- read.csv(unz("activity.zip","activity.csv"))
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
Datos$date <- as.Date(Datos$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
### **Making the sum of steps for each day, we obtain a vector of total values for steps, on this vector we calculate the mean and the median.**

### 1. Make a histogram of the total number of steps taken each day


```r
steps.date.sin.na <- Datos %>% filter(!is.na(steps)) %>% group_by(date) %>% summarize_at(c("steps"), sum) %>% rename(T.sum = steps)
head(steps.date.sin.na)
```

```
## # A tibble: 6 x 2
##   date       T.sum
##   <date>     <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```


```r
hist(steps.date.sin.na$T.sum, main = "Total number of steps taken each day", xlab = "Steps")
```

![](figure/histWna-1.png)<!-- -->

### 2. Calculate and report the mean and median total number of steps taken per day


```r
mean(steps.date.sin.na$T.sum)
```

```
## [1] 10766.19
```

```r
median(steps.date.sin.na$T.sum)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
### **Throughout each day there is a tendency to do more steps (more exercise) that is located in the interval 835, where the average of every day exceeds the 200 steps.**

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
DT.mean <- Datos %>% filter(!is.na(steps)) %>% group_by(interval) %>% summarize_at(c("steps"), mean) %>% rename(Mean.steps = steps)
plot(DT.mean$interval, DT.mean$Mean.steps, type = "l", main = "The mean of steps (all days) taken each 5-minute interval", xlab = "5-minute interval", ylab = "Mean steps (all days)")
```

![](figure/plotTS-1.png)<!-- -->

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
DT.mean$interval[DT.mean$Mean.steps == max(DT.mean$Mean.steps)]
```

```
## [1] 835
```

## Imputing missing values

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
head(steps.date.imp.na)
```

```
## # A tibble: 6 x 2
##   date        T.sum
##   <date>      <dbl>
## 1 2012-10-01 10766.
## 2 2012-10-02   126 
## 3 2012-10-03 11352 
## 4 2012-10-04 12116 
## 5 2012-10-05 13294 
## 6 2012-10-06 15420
```


```r
hist(steps.date.imp.na$T.sum, main = "Total number of steps taken each day - impute NA", xlab = "Steps")
```

![](figure/histImpute-1.png)<!-- -->
 

```r
mean(steps.date.imp.na$T.sum)
```

```
## [1] 10766.19
```

```r
median(steps.date.imp.na$T.sum)
```

```
## [1] 10766.19
```

### Do these values differ from the estimates from the first part of the assignment? 
### **There is no difference between these values and the values of the principle.**

### What is the impact of imputing missing data on the estimates of the total daily number of steps?
### **The imputed values increase the central value (mean, median), with which there is no real impact.**

## Are there differences in activity patterns between weekdays and weekends?
### **Yes, there are. There is a certain tendency to make more steps (more exercise) the weekdays at a certain time of day, that the days of the weekend. But at the weekend the trend is more constant throughout the day, in other words, they are made more steps (more exercise) in more intervals of the day**

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

![](figure/xyplot-1.png)<!-- -->
