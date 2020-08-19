activity <- read.csv('activity.csv') 
##read the csv file to a variable called activity

library(dplyr)
library(ggplot2)

activity_no_na <- na.omit(activity)
activity_no_na$date <- as.Date(activity_no_na$date, "%Y-%m-%d")


activity_daily_no_na <- activity_no_na %>% group_by(date) %>% summarize(steps = sum(steps))
##convert the data to daily basis

hist(activity_daily_no_na$steps, ylim = c(0, 30), 
     xlab = "Daily Steps", ylab = "Number of days",
     main = "Histogram of Daily Steps")
steps_mean <- mean(activity_daily_no_na$steps, na.rm = TRUE)
steps_median <- median(activity_daily_no_na$steps, na.rm = TRUE)

activity_im <- activity %>% group_by(interval) %>% summarize(steps = mean(steps, na.rm = TRUE))
with(activity_im, plot(interval, steps, type = "l"))

## interval with highest steps mean
max_steps <- max(activity_im$steps)
interval <- activity_im[activity_im$steps == max_steps, 'interval']

num_na <- sum(is.na(activity$steps))

activity_filled <- activity
for (i in 1:nrow(activity)){
    if (is.na(activity_filled[i,'steps'])){
        activity_filled[i,'steps'] <- round(activity_im[activity_im$interval == activity_filled[i,'interval'], 'steps'])
    }
}