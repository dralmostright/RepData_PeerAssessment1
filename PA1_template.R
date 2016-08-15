# Coursera D.S Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data
filename <- "/R/workspace/rr-week2/dataset.zip"

if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, filename, method="curl")
}  

if (!file.exists("/R/workspace/rr-week2/activity.csv")) { 
  unzip(filename, exdir='/R/workspace/rr-week2') 
}

# Load the dataset in memory
data <- read.csv('activity.csv')

## Mean total number of steps taken per day
aggregatedTotalSteps <- aggregate(steps ~ date, data, sum)
hist(aggregatedTotalSteps$steps, breaks=30, main="Total Steps taken per Day", xlab="Steps", ylab="Frequency")

# Calculate Median
median(aggregatedTotalSteps$steps)

#Calculate Mean
mean(aggregatedTotalSteps$steps)


## What is the average daily activity pattern?

aggregateStepsInterval <- aggregate(steps ~ interval, data, mean)
plot(aggregateStepsInterval$interval,aggregateStepsInterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps tanken \n per Day by Interval")

aggregateStepsInterval[aggregateStepsInterval$steps==max(aggregateStepsInterval$steps),1]


## Imputing missing values
sum(is.na(data$steps))

#data$interval <- as.factor(data$interval)
tempIntervalmean <- aggregate(steps ~ interval, data, mean)
tempIntervalmean$steps <- ceiling(tempIntervalmean$steps)

head(tempIntervalmean,10)
for(i in 1:nrow(data)){
	if(is.na(data[i,1]))
		data[i,1] <- tempIntervalmean$steps[tempIntervalmean$interval == data[i,3]]
}


#Make a histogram of the total number of steps taken each day after Imputing missing values


aggregatedTotalSteps <- aggregate(steps ~ date, data, sum)
hist(aggregatedTotalSteps$steps, breaks=30, main="Total Steps taken per Day\n After data cleaning", xlab="Steps", ylab="Frequency")

# Calculate Median
median(aggregatedTotalSteps$steps)

#Calculate Mean
mean(aggregatedTotalSteps$steps)


## Are there differences in activity patterns between weekdays and weekends?

library(ggplot2)
data$daytype <- 0

data$date <- as.Date(data$date)
for(i in 1:nrow(data)){
	if(weekdays(data[i,2]) %in% c('Sunday', 'Saturday'))
		data[i,4] <- "weekend"
	else
		data[i,4] <- "weekday"
}

#data$daytype <- as.factor(data$daytype)
#data$interval <- as.numeric(data$interval)

aggDataWeetype <- aggregate(steps ~ interval + daytype, data, mean)

aggDataWeektype <- aggregate(steps ~ interval + daytype, data=data, mean)
ggplot(aggDataWeektype, aes(interval, steps)) + geom_line( aes(color = daytype, group=daytype)) + facet_grid(daytype ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
