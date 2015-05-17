---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
echo = TRUE

library(lattice)
library(plyr)
```

## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

## What is mean total number of steps taken per day?

```r
totalSteps <- ddply(data, "date", summarize, numSteps = sum(steps))
hist(totalSteps$numSteps
     , main = "Total Steps"
     , xlab = "Total Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
meanSteps <- mean(totalSteps$numSteps, na.rm = TRUE)
medianSteps <- median(totalSteps$numSteps, na.rm = TRUE)

meanSteps
```

```
## [1] 10766.19
```

```r
medianSteps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
dailyPattern <- ddply(data, "interval", summarize, avgSteps = mean(steps, na.rm = TRUE))
plot(dailyPattern$avgSteps
     , type = "l"
     , main = "Daily Steps"
     , xlab = "5-Minute Increments"
     , ylab = "Avg Number of Steps per Day"
     , col = "blue"
     , xaxt = "n")
axis(1, 1:nrow(dailyPattern),dailyPattern$interval)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
maxInterval <- dailyPattern[dailyPattern$inerval == max(dailyPattern$interval),]
maxInterval
```

```
## [1] interval avgSteps
## <0 rows> (or 0-length row.names)
```


## Imputing missing values

```r
numNA <- sum(is.na(data$steps))
numNA
```

```
## [1] 2304
```

```r
noNAData <- data
noNAData$steps[is.na(noNAData$steps)] <- 0
noNATotalSteps <- ddply(noNAData, "date", summarize, numSteps = sum(steps))
hist(noNATotalSteps$numSteps
     , main = "Total Steps Without NAs"
     , xlab = "Total Steps Without NAs")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
noNAMean <- mean(noNAData$steps)
noNAMedian <- median(noNAData$steps)
noNAMean
```

```
## [1] 32.47996
```

```r
noNAMedian
```

```
## [1] 0
```


## Are there differences in activity patterns between weekdays and weekends?

```r
data$date <- as.POSIXlt(data$date, format = "%Y-%m-%d")
data$day <- weekdays(data$date)
data$typeDay <- ifelse(data$day == "Saturday" | data$day == "Sunday"
                       , "Weekend"
                       , "weekday")
data$typeDay <- factor(data$typeDay)


weekdayGroups <- ddply(data
                       , ~interval + typeDay
                       , summarise
                       , mean = mean(steps, na.rm=TRUE))

weekdayGroups$interval <- as.numeric(as.character(weekdayGroups$interval))
xyplot(mean ~ interval | typeDay
       , weekdayGroups
       , type = "l"
       , layout = c(1, 2)
       , xlab = "Interval"
       , ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 
