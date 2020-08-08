##Reproducible Research - Week 2

## Loading and preprocessing the data
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
head(data)

## What is mean total number of steps taken per day?
library(magrittr)
library(dplyr)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
hist(total.steps,
     breaks = 20,
     col="wheat3",
     xlab = "Steps taken",
     main = "total number of steps taken per day")
mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)


## What is the average daily activity pattern?
steps <- tapply(data$steps,data$interval,FUN = mean, na.rm=TRUE) 
interval <- as.array(unique(data$interval))
subdata <- data.frame(interval,steps)
plot(interval,steps,
     type = 'l',
     main = "average daily activity pattern")
subdata[which.max(subdata$steps),]

##Imputing missing values
sum(is.na(data))

library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- data%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)

FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

summary(FullSummedDataByDay)

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
mean(FullSummedDataByDay$totalsteps)

## Are there differences in activity patterns between weekdays and weekends?
meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
        facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
        ggtitle("Comparison of Average Number of Steps in Each Interval")

