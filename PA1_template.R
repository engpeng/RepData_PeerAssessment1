## Loading and preprocessing the data

#load data
echo = TRUE
activity <- NULL
activity <- read.csv("activity.csv", header = T, sep = ",")

#process data into suitable format for analysis
echo = TRUE
activity$timeanddate <- as.POSIXct(
  with(
    activity,
    paste(
      date,
      paste(interval %/% 100, interval %% 100, sep=":"))
  ),
  format="%Y-%m-%d %H:%M",tz="")

#libraries needed for plots
echo = TRUE
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)

## What is mean total number of steps taken per day?

#calculating data for histogram
echo = TRUE
stepperday<- setNames(
  aggregate(
    steps~as.Date(date),
    activity,
    sum,
    na.rm = TRUE),
  c("date","steps")
)

#plotting histogram
echo = TRUE
hist1 <- ggplot(stepperday,aes(x=date,y=steps)) + 
  geom_bar(stat="identity") + 
  ggtitle("Total number of steps per day (source data)")
print(hist1)

#find mean and media
echo = TRUE
meanandmed <- c(mean = mean(stepperday$steps),median = median(stepperday$steps))
print(meanandmed)

## What is the average daily activity pattern?

#time series plot
echo = TRUE
avg <- aggregate(steps~interval,activity,mean,na.rm = TRUE)
avg$time <- as.POSIXct(with(avg,paste(interval %/% 100, interval %% 100, sep=":")),format="%H:%M")
plot <- ggplot(avg,aes(x=time,y=steps)) + 
  geom_line() + 
  scale_x_datetime(breaks = date_breaks("2 hour"),labels = date_format("%H:%M"))
print(plot)

#max number of steps
echo = TRUE
with(avg,avg[steps == max(steps),])

##Imputing missing values

#number of missing values
echo = TRUE
NAvalue <- aggregate(cnt~date,cbind(activity[is.na(activity$steps),],cnt=c(1)),sum,na.rm = FALSE)
NAvalue$dow <- weekdays(as.Date(NAvalue$date),abbreviate=TRUE)
print(NAvalue[,c(1,3,2)])

#filling in NA values
echo = TRUE
unique(NAvalue$dow)
newset <- aggregate(steps~interval+weekdays(timeanddate,abbreviate=TRUE),activity,FUN=mean,na.rm=TRUE)
colnames(newset) <- c("interval","dow","avg_steps")
newset$dow <- factor(newset$dow,levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
ggplot(newset,aes(x=interval,y=avg_steps)) + geom_line() + facet_grid("dow ~ .")

#full data set
echo = TRUE
activity$dow <- weekdays(activity$timeanddate,abbreviate=TRUE)
merged <- merge(activity,newset,by=c("dow","interval"),all.x = TRUE)
merged <- merged[with(merged,order(date,interval)),]
merged$fixed_steps <- ifelse(is.na(merged$steps),merged$avg_steps,merged$steps)

#histogram of total steps each day, mean and median
# calculating data set for histogram
echo = TRUE
NEWstepperday <- setNames(
  aggregate(
    fixed_steps~as.Date(date),
    merged,
    sum,
    na.rm = TRUE),
  c("date","steps")
)

# plotting histogram
echo = TRUE
hist2 <- ggplot(NEWstepperday,aes(x=date,y=steps)) + 
  geom_bar(stat="identity") + 
  ggtitle("Total number of steps per day (fixed data)")

# combining with previous
echo = TRUE
grid.arrange(hist1, hist2, nrow=2)
NEWmeanandmed <- c(mean = mean(NEWstepperday$steps),median = median(NEWstepperday$steps))
difference <- rbind(source = meanandmed, fixed = NEWmeanandmed, delta = NEWmeanandmed-meanandmed)
print(difference)

##Differences between weekdays and weekends

#create factor vairable
echo = TRUE
week_diff <- aggregate(
  steps~dow+interval,  # group steps by weekend/weekday and interval to find average steps 
  with(
    activity,
    data.frame(
      dow = factor(
        ifelse(
          weekdays(as.Date(date)) %in% c("Sunday","Saturday"),
          "weekend",  # if sunday or saturday
          "weekday"   # else
        )
      ),
      interval,
      steps
    )
  ),
  FUN = mean,
  rm.na = TRUE
)

#time series
echo = TRUE
ggplot(week_diff,aes(x=interval,y=steps)) + geom_line() + facet_grid("dow ~ .")


##processing r markdown file
knit2html("PA1_template.Rmd")