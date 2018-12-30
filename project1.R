library(ggplot2)

setwd("~/coursera")

projectdata <- read.csv('activity.csv')

stepsByDay <- tapply(projectdata$steps, projectdata$date, sum, na.rm=TRUE)
mean(stepsByDay)
steps_per_interval <- aggregate(steps ~ interval, data = projectdata, mean, na.rm = TRUE)
missing <- !complete.cases(projectdata)
projectdata$dateType <-  ifelse(as.POSIXlt(projectdata$date)$wday %in% c(0,6), 'weekend', 'weekday')

qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 1000', binwidth=1000)

plot(steps ~ interval, data = steps_per_interval, type = "l")

sum(missing == TRUE)

averagedActivityPatterns <- aggregate(steps ~ interval + dateType, data=projectdata, mean)
ggplot(averagedActivityPatterns, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("average number of steps")