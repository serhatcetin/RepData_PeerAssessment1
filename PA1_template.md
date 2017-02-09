# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
1 - Load the raw activity data
```{r loaddata}
activity_raw <- read.csv("activity.csv", stringsAsFactors=FALSE)
```
2 - Process/transform the data (if necessary) into a format suitable for analysis

Transform the date attribute to an actual date format
```{r}
activity_raw$date <- as.POSIXct(activity_raw$date, format="%Y-%m-%d")
```
Compute the weekdays from the date attribute
```{r}
activity_raw <- data.frame(date=activity_raw$date, 
                           weekday=tolower(weekdays(activity_raw$date)), 
                           steps=activity_raw$steps, 
                           interval=activity_raw$interval)
```
Compute the day type (weekend or weekday)
```{r}
activity_raw <- cbind(activity_raw, 
                      daytype=ifelse(activity_raw$weekday == "saturday" | 
                                       activity_raw$weekday == "sunday", "weekend", 
                                     "weekday"))
```
Create the final data.frame
```{r}
activity <- data.frame(date=activity_raw$date, 
                       weekday=activity_raw$weekday, 
                       daytype=activity_raw$daytype, 
                       interval=activity_raw$interval,
                       steps=activity_raw$steps)
```
Display the first few rows of the activity data frame:
```{r}
head(activity)
```

## What is mean total number of steps taken per day?

1 - Make a histogram of the total number of steps taken each day

Compute the total number of steps each day (NA values removed)
```{r}
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum, na.rm=TRUE)
```
Rename the attributes
```{r}
names(sum_data) <- c("date", "total")
```
Display the first few rows of the sum_data data frame:
```{r}
head(sum_data)
```
Compute the histogram of the total number of steps each day
```{r}
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 20), 
     main="Histogram of the total number of steps taken each day\n(NA removed)")
```
2 -Calculate and report the mean and median total number of steps taken per day
```{r}
mean(sum_data$total)
median(sum_data$total)
```
mean(sum_data$total): 9354.23
median(sum_data$total): 10395


## What is the average daily activity pattern?

1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Compute the means of steps accross all days for each interval
```{r}
mean_data <- aggregate(activity$steps, 
                       by=list(activity$interval), 
                       FUN=mean, 
                       na.rm=TRUE)
```
Rename the attributes
```{r}
names(mean_data) <- c("interval", "mean")
```
Display the first few rows of the mean_data data frame:
```{r}
head(mean_data)
```
Compute the time series plot
```{r}
plot(mean_data$interval, 
     mean_data$mean, 
     type="l", 
     col="blue", 
     lwd=2, 
     xlab="Interval [minutes]", 
     ylab="Average number of steps", 
     main="Time-series of the average number of steps per intervals\n(NA removed)")
```

##2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

We find the position of the maximum mean
```{r}
max_pos <- which(mean_data$mean == max(mean_data$mean))
```
We lookup the value of interval at this position
```{r}
max_interval <- mean_data[max_pos, 1]
```
The 5-minute interval that contains the maximum of steps, on average across all days, is 835.


## Imputing missing values

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

1 - Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA's)

We use the trick that a TRUE boolean value is equivalent to 1 and a FALSE to 0.
```{r how}
NA_count <- sum(is.na(activity$steps))
#The number of NA's is 2304
```
2 -Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Find the NA positions
```{r}
na_pos <- which(is.na(activity$steps))
```
Create a vector of means
```{r}
mean_vec <- rep(mean(activity$steps, na.rm=TRUE), times=length(na_pos))
```
3 - Create a new dataset that is equal to the original dataset but with the missing data filled in.
Replace the NAs by the means
```{r}
activity[na_pos, "steps"] <- mean_vec
```
Display the first few rows of the new activity data frame
```{r}
head(activity)
```
4 - Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Compute the total number of steps each day (NA values removed)
```{r}
sum_data <- aggregate(activity$steps, by=list(activity$date), FUN=sum)
```
Rename the attributes
```{r}
names(sum_data) <- c("date", "total")
```
Compute the histogram of the total number of steps each day
```{r}
hist(sum_data$total, 
     breaks=seq(from=0, to=25000, by=2500),
     col="blue", 
     xlab="Total number of steps", 
     ylim=c(0, 30), 
     main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")
```



## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


1 - Create a new factor variable in the dataset with two levels - "weekdays" and "weekend" indicating whether a given date is a weekday or weekend day.

The new factor variable "daytype" was already in the activity data frame
```{r}
head(activity)
```
2 - Make a panel plot containing a time series plot (i.e. type = "l") of the 5- minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Load the lattice graphical library
```{r}
library(lattice)
```
Compute the average number of steps taken, averaged across all daytype variable
```{r}
mean_data <- aggregate(activity$steps, 
                       by=list(activity$daytype, 
                               activity$weekday, activity$interval), mean)
```
Rename the attributes
```{r}
names(mean_data) <- c("daytype", "weekday", "interval", "mean")
```
Display the first few rows of the mean_data data frame
```{r}
head(mean_data)
```
Compute the time serie plot
```{r}
xyplot(mean ~ interval | daytype, mean_data, 
       type="l", 
       lwd=1, 
       xlab="Interval", 
       ylab="Number of steps", 
       layout=c(1,2))
```
From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays (probably because the oject is working during the weekdays, hence moving less during the day).

