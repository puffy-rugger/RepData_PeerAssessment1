library(ggplot2)

# Load "activity.csv" file.
dt <- read.csv("../activity.csv")

dt$date <- as.Date(dt$date, "%Y-%m-%d")

#What is mean total number of steps taken per day?
#Calculate the total number of steps taken per day
a <- aggregate(steps ~ date, data = dt, na.rm = TRUE, sum)


#hist(a$steps)
#If you do not understand the difference between a histogram and a barplot, 
#research the difference between them. Make a histogram of the total number of 
#steps taken each day
g <- ggplot(data = a, aes(steps))

g <- g + 
     geom_histogram (stat = "bin", bins = 20, colour = "#000000") + 
     xlab("Date") + 
     ylab("Frequency") +
     ggtitle("Total Steps") +
     theme_light()

print(g)


#Calculate and report the mean and median of the total number of steps 
#taken per day mean of steps taken
mean(a$steps)

#median of steps taken
median(a$steps)

##What is the average daily activity pattern?

#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all days (y-axis)
a <- aggregate(steps ~ interval, data = dt, na.rm = TRUE, mean)

plot(a$interval, a$steps, type = "l", xlab = "Interval", ylab = "Steps Taken", 
     main = "Steps taken at 5 min interval", col = "blue")

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
a[which.max(a$steps),]$interval

#Imputing missing values
#Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
sum(is.na(dt$steps))

#Devise a strategy for filling in all of the missing values in the dataset. 
#The strategy does not need to be sophisticated. For example, you could use the 
#mean/median for that day, or the mean for that 5-minute interval, etc.
# Load "activity.csv" file.
dt <- read.csv("../activity.csv")

dt$date <- as.Date(dt$date, "%Y-%m-%d")

nas <- which(is.na(dt$steps))

dtt <- mutate(dt, bin = ((dt$interval %% 60)/5)+1)
means <- aggregate(steps ~ bin, dtt, mean)

for (t in nas) {
    dt$steps[t] <- means$steps[dtt$bin[t]]
}


#Create a new dataset that is equal to the original dataset but with the missing 
#data filled in.

#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? What is the 
#impact of imputing missing data on the estimates of the total daily number 
#of steps?

par(mfrow = c(2,1))
totalsteps <- aggregate(steps ~ date, dt, sum)
hist(totalsteps$steps, 
     breaks = 20, 
     main = "Total Steps taken per day (Mean)", 
     xlab = "Steps",
     col = "lightgreen",
     border = "green")
abline(v = mean(totalsteps$steps))

totalsteps <- aggregate(steps ~ date, dt, sum)
hist(totalsteps$steps, 
     breaks = 20, 
     main = "Total Steps taken per day (Median)", 
     xlab = "Steps",
     col = "lightblue",
     border = "blue")
abline(v = median(totalsteps$steps))

par(mfrow = c(1,1))

## Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels - "weekday" 
#and "weekend" indicating whether a given date is a weekday or weekend day.
dt <- read.csv("../activity.csv")

dt$date <- as.Date(dt$date, "%Y-%m-%d")

dt$day <- ""

dt$day[!(weekdays(dt$date) == "Saturday" | weekdays(dt$date) == "Sunday")] <- 
    "Weekday"

dt$day[weekdays(dt$date) == "Saturday" | weekdays(dt$date) == "Sunday"] <- 
    "Weekend"

dt$day <- as.factor(dt$day)
#Make a panel plot containing a time series plot (i.e. type = "l") of the 
#5-minute interval (x-axis) and the average number of steps taken, averaged 
#across all weekday days or weekend days (y-axis). 
#See the README file in the GitHub repository to see an example of what this 
#plot should look like using simulated data.

dtt <- aggregate(steps ~ interval + day, dt, mean, na.rm = TRUE)

xyplot(steps ~ interval | day, 
       dtt, 
       type = "l", 
       layout = c(1,2),
       main = "Average number of steps taken (Weeday vs Weekend)",
       xlab = "Interval",
       ylab = "Steps",
       col = "blue")

g <- ggplot(dtt, aes(interval, steps))
g <- g + 
    facet_grid(. ~day) +
    geom_line(color = "steelblue") +
    theme_light()


print(g)