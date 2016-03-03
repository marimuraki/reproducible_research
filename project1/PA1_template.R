# Source: https://github.com/marimuraki/reproducible_research

rm(list=ls())

library(ggplot2)

setwd("/Users/marimuraki/Dropbox/Mari/courses/Coursera/Reproducible Research/project1")

url    <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zip    <- "./data/repdata-data-activity.zip" 
file   <- "./data/activity.csv"

if (!file.exists(zip)) {
  download.file(url, 
                destfile=zip)
}

if (!file.exists(file)) {
  unzip(zip, 
        exdir="./data")
  file.remove(zip)
}

data <- read.csv(file, 
                 sep=",",
                 na.string = "NA",
                 header=TRUE)

data$date_fmt <- as.Date(data$date, format="%Y-%m-%d")

# Q1: What is mean total number of steps taken per day?

# Make a histogram of the total number of steps taken each day

total_steps <- tapply(data$steps, 
                      data$date, 
                      FUN=sum, 
                      na.rm=TRUE)

png(file="plot1.png")
qplot(total_steps,
      geom="histogram",
      binwidth=1000,
      xlab="Number of steps",
      main="Total Number of Steps per Day")
dev.off()

# Calculate the mean and median total number of steps taken per day

mean_total_steps    <- mean(total_steps, na.rm=TRUE)
median_total_steps  <- median(total_steps, na.rm=TRUE)

# Q2: What is the average daily activity pattern?

# mean_steps_per_interval <- tapply(data$steps, 
#                                    data$interval, 
#                                    FUN=mean, 
#                                    na.rm=TRUE)

mean_steps_per_interval <- aggregate(steps~interval,
                                     data=data,
                                     FUN=mean,
                                     na.rm=TRUE)

# Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

png(file="plot2.png")
ggplot(mean_steps_per_interval, aes(interval, steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("Number of steps") +
  ggtitle("Average Number of Steps per 5-min Intervals")
dev.off()

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

mean_steps_per_interval[which.max(mean_steps_per_interval$steps),]

# Q3: Imputing missing values

# Calculate the total number of missing values in the dataset

table(is.na(data$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

impute_steps <- function(steps, interval) {
  new_steps <- steps
  if(is.na(new_steps))
    new_steps <- mean_steps_per_interval[mean_steps_per_interval$interval==interval, "steps"]
  return(new_steps)
}

# Create a new dataset that is equal to the original dataset but with the missing data filled in.

imputed_data <- data
imputed_data$steps <- mapply(impute_steps, 
                             imputed_data$steps, 
                             imputed_data$interval)

# Make a histogram of the total number of steps taken each day. 

imputed_total_steps <- tapply(imputed_data$steps, 
                              imputed_data$date, 
                              FUN=sum, 
                              na.rm=TRUE)

png(file="plot3.png")
qplot(imputed_total_steps,
      binwidth=1000,
      xlab="Number of steps",
      main="Total Number of Steps per Day \n(imputed values)")
dev.off()

# Calculate the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

mean_imputed_total_steps    <- mean(imputed_total_steps, na.rm=TRUE)
median_imputed_total_steps  <- median(imputed_total_steps, na.rm=TRUE)

# Q4: Are there differences in activity patterns between weekdays and weekends?

# Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

weekday_or_weekend <- function(date) {
  day <- weekdays(date)
  if(day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    return("weekday")
}

imputed_data$day <- sapply(imputed_data$date_fmt, FUN=weekday_or_weekend)

# Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis).

mean_steps_per_intervalday <- aggregate(steps~interval + day,
                                       data=imputed_data,
                                       FUN=mean,
                                       na.rm=TRUE)

png(file="plot4a.png")
ggplot(mean_steps_per_intervalday, aes(interval, steps)) +
  geom_line() +
  facet_wrap(~day, nrow=2) +
  xlab("5-minute interval") +
  ylab("Number of steps") +
  ggtitle("Average Number of Steps per 5-min Intervals")
dev.off()

png(file="plot4b.png")
ggplot(mean_steps_per_intervalday, aes(interval, steps)) +
  geom_line(aes(linetype=day, colour=day)) +
  xlab("5-minute interval") +
  ylab("Number of steps") +
  ggtitle("Average Number of Steps per 5-min Intervals")
dev.off()
