## Loading and preprocessing the data
unzip(zipfile="activity.zip")
data <- read.csv("activity.csv")

## What is mean total number of steps taken per day?
library(ggplot2)
stepsperday <- tapply(data$steps, data$date, FUN=sum, na.rm = TRUE)
png(filename = "plot1.png")
qplot(stepsperday, binwidth=1000)
dev.off()
mean(stepsperday, na.rm = TRUE)
median(stepsperday, na.rm = TRUE)

## What is the average daily activity pattern?
averageactivity <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
png(filename = "plot2.png")
ggplot(data=averageactivity, aes(x=interval, y=steps)) + geom_line()
dev.off()

averageactivity[which.max(averageactivity$steps),]



## Imputing missing values
missingvalues <- is.na(data$steps)
table(missingvalues)
meanvalue <- function(steps, interval) {
  filled <- NA
  ifelse (!is.na(steps), filled <- c(steps), filled <- (averageactivity[averageactivity$interval==interval, "steps"]))
  return(filled)
}
completedata <- data
completedata$steps <- mapply(meanvalue, completedata$steps, completedata$interval)

totalsteps <- tapply(completedata$steps, completedata$date, FUN=sum)

png(filename = "plot3.png")
qplot(totalsteps, binwidth=1000)
dev.off()

mean(totalsteps)
median(totalsteps)




## Are there differences in activity patterns between weekdays and weekends?
dayofweek <- function(date) {
  day <- weekdays(date)
  ifelse (day == "Saturday" | day == "Sunday", "Weekend", "Weekday")
}
completedata$date <- as.Date(completedata$date)
completedata$day <- sapply(completedata$date, FUN=dayofweek)

averageweekday <- aggregate(steps ~ interval + day, data=completedata, mean)
png(filename = plot4.png)
ggplot(averageweekday, aes(interval, steps)) + geom_line() + facet_grid(day ~ .)
dev.off()