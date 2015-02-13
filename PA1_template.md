# Reproducible Research: Peer Assessment 1

## Loading the library

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
library(ggplot2)
```


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
## Make a histogram of the total number of steps taken each day
# Ignore missing value
activity_ignore_na_steps <- filter(activity, !is.na(steps))
by_date <- group_by(activity_ignore_na_steps, date)
sum_steps_per_date <- summarise(by_date, sum_of_steps = sum(steps))
# histogram is a function of lattice library
histogram(~ sum_of_steps, data = sum_steps_per_date, xlab = "Sum of steps per date", main = "Histogram of the total number of steps taken each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
## Calculate and report the mean and median total number of steps taken per day
summarise(sum_steps_per_date, "Mean total" = mean(sum_of_steps), "Median total" = median(sum_of_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   Mean total Median total
## 1   10766.19        10765
```


## What is the average daily activity pattern?

```r
## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
# Ignore missing value
by_interval <- group_by(activity_ignore_na_steps, interval)
mean_steps_per_interval <- summarise(by_interval, mean_of_steps = mean(steps))
g <- ggplot(mean_steps_per_interval, aes(interval, mean_of_steps))
g + geom_line() + xlab("The 5-minute interval") + ylab("The average of steps taken") + labs(title = "The average daily activity pattern")
```

![](./PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
arrange(mean_steps_per_interval, desc(mean_of_steps)) %>% head(1)
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean_of_steps
## 1      835      206.1698
```


## Imputing missing values

```r
## Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
activity_na <- activity[is.na(activity$steps), ]
nrow_na <- nrow(activity_na)
nrow_na
```

```
## [1] 2304
```

```r
## Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
mean_steps_ignore_na <- mean(activity_ignore_na_steps$steps)

## Create a new dataset that is equal to the original dataset but with the missing data filled in.
for(i in 1:nrow_na) {
    activity_na$steps <- mean_steps_ignore_na
}
activity_new <- rbind(activity_na, activity_ignore_na_steps)

## Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
# Make a histogram of the total number of steps taken each day
by_date <- group_by(activity_new, date)
sum_steps_per_date <- summarise(by_date, sum_of_steps = sum(steps))
histogram(~ sum_of_steps, data = sum_steps_per_date, xlab = "Sum of steps per date", main = "Histogram of the total number of steps taken each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
# Calculate and report the mean and median total number of steps taken per day.
summarise(sum_steps_per_date, "Mean total" = mean(sum_of_steps), "Median total" = median(sum_of_steps))
```

```
## Source: local data frame [1 x 2]
## 
##   Mean total Median total
## 1   10766.19     10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

```r
## For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
## Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity_new$weekday <- as.factor(ifelse(weekdays(as.Date(activity_new$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))

## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:
data <- summarise(group_by(activity_new, interval, weekday), mean_steps = mean(steps))
g2 <- ggplot(data, aes(interval, mean_steps))
g2 + facet_grid(weekday ~ .) + geom_line() + labs(title = "The time series plot of interval and the average of steps taken") + ylab("The average number of steps")
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 



