# Reproducible Research: Project 1


1) Loading and preprocessing the data
--------------------------------------


```r
#setwd("~/RStudio/data")
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
datafile <- "step_data.zip"
download.file(url, datafile)
unzip(datafile)
activityData <- read.csv("activity.csv")
```

2) Mean of "Total number of step taken per day" over all days
-------------------------------------------------------------


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
total.steps <- tapply(activityData$steps, activityData$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
mean(total.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total.steps, na.rm=TRUE)
```

```
## [1] 10395
```

3) Average daily activity pattern
---------------------------------


```r
library(ggplot2)
avgDaily <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=avgDaily, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?

```r
avgDaily[which.max(avgDaily$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

4) Imputing missing values
--------------------------

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
imputeMiss <- is.na(activityData$steps)

# How many values missing

table(imputeMiss)
```

```
## imputeMiss
## FALSE  TRUE 
## 15264  2304
```

All of the missing values are filled in with mean value for that 5-minute interval.


```r
# Replace each missing value with the mean value of its 5-minute interval

imputeFill.value <- function(steps, interval) {
    fillDF <- NA
    if (!is.na(steps))
        fillDF <- c(steps)
    else
        fillDF <- (avgDaily[avgDaily$interval==interval, "steps"])
    return(fillDF)
}
fillDF.data <- activityData
fillDF.data$steps <- mapply(imputeFill.value, fillDF.data$steps, fillDF.data$interval)
```
let's make a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.


```r
total.steps <- tapply(fillDF.data$steps, fillDF.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

5) Differences in activity patterns: Weekdays vs Weekends
---------------------------------------------------------

First, let's find the day of the week for each measurement in the dataset. In this part, we use the dataset with the filled-in values.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
fillDF.data$date <- as.Date(fillDF.data$date)
fillDF.data$day <- sapply(fillDF.data$date, FUN=weekday.or.weekend)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.


```r
averages <- aggregate(steps ~ interval + day, data=fillDF.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
