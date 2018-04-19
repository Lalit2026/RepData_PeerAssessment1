library(ggplot2)

Set the working Directory
-------------------------

setwd("C:/Users/Lalit2026/Desktop/Data Science")

Loading and preprocessing the data
----------------------------------

activity &lt;- NULL activity &lt;- read.csv("activity.csv", header = T,
sep = ",")

Print some lines from activity
------------------------------

head(activity)

First, the total (sum) of steps is determined for every single date
-------------------------------------------------------------------

su &lt;- tapply(activity*s**t**e**p**s*, *a**c**t**i**v**i**t**y*date,
sum, na.rm=T)

Plot the Histogram
------------------

hist(su, xlab = "sum of steps per day", main = "histogram of steps per
day")

The mean and the median total number of steps taken per day are reported:
-------------------------------------------------------------------------

mean\_su &lt;- round(mean(su)) median\_su &lt;- round(median(su))
print(c("The mean is",mean\_su))

print(c("The median is",median\_su))

A time series plot of the 5-minute interval and the average number of steps taken (averaged across all days) is shown below:
----------------------------------------------------------------------------------------------------------------------------

mn\_int &lt;-
tapply(activity*s**t**e**p**s*, *a**c**t**i**v**i**t**y*interval, mean,
na.rm=T) plot(mn\_int ~ unique(activity$interval), type="l", xlab =
"5-min interval")

The 5-minute interval (on average across all the days in the dataset) that contains the maximum number of steps is the following (below are shown the interval showing the max. number of steps and the value of the max. number of steps):
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

mn\_int\[which.max(mn\_int)\]

Imputing missing values
-----------------------

table(is.na(activity) == TRUE)

summary(activity)

activity2 &lt;- activity \# creation of the dataset that will have no
more NAs for (i in 1:nrow(activity)){
if(is.na(activity$steps\[i\])){  activity2$steps\[i\]&lt;-
mn\_int\[\[as.character(activity\[i, "interval"\])\]\] } }

Below is a histogram of the total number of steps taken each day. The mean and median total number of steps taken per day are reported.
---------------------------------------------------------------------------------------------------------------------------------------

su2 &lt;-
tapply(activity2*s**t**e**p**s*, *a**c**t**i**v**i**t**y*2date, sum,
na.rm=T) hist(su2, xlab = "sum of steps per day", main = "histogram of
steps per day")

mean\_su2 &lt;- round(mean(su2)) median\_su2 &lt;- round(median(su2)

The new values are :
--------------------

print(c("The mean is",mean\_su2)) print(c("The median is",median\_su2))

In order to compare the new values with the "old" values:
---------------------------------------------------------

df\_summary &lt;- rbind(df\_summary, data.frame(mean = c(mean\_su,
mean\_su2), median = c(median\_su, median\_su2))) rownames(df\_summary)
&lt;- c("with NA's", "without NA's") print(df\_summary)

For comparison with NA's and without (see earlier):
---------------------------------------------------

summary(activity2)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

activity2$weekday &lt;- c("weekday") activity2\[weekdays(as.Date(activity2\[, 2\])) %in% c("Saturday", "Sunday", "samedi", "dimanche", "saturday", "sunday", "Samedi", "Dimanche"), \]\[4\] &lt;- c("weekend") table(activity2$weekday
== "weekend")

activity2*w**e**e**k**d**a**y* &lt; −*f**a**c**t**o**r*(*a**c**t**i**v**i**t**y*2weekday)

In order to visualize the difference bewteen weekends and days of the week, a new dataframe is created to be usable by the lattice package. First, the data are calculated:
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

activity2\_weekend &lt;- subset(activity2,
activity2$weekday == "weekend") activity2\_weekday &lt;- subset(activity2, activity2$weekday
== "weekday")

mean\_activity2\_weekday &lt;-
tapply(activity2\_weekday*s**t**e**p**s*, *a**c**t**i**v**i**t**y*2<sub>*w*</sub>*e**e**k**d**a**y*interval,
mean) mean\_activity2\_weekend &lt;-
tapply(activity2\_weekend*s**t**e**p**s*, *a**c**t**i**v**i**t**y*2<sub>*w*</sub>*e**e**k**e**n**d*interval,
mean)

Then the dataframe is prepared and the plot is. plotted !
---------------------------------------------------------

library(lattice) df\_weekday &lt;- NULL df\_weekend &lt;- NULL df\_final
&lt;- NULL df\_weekday &lt;- data.frame(interval =
unique(activity2\_weekday$interval), avg = as.numeric(mean\_activity2\_weekday), day = rep("weekday", length(mean\_activity2\_weekday))) df\_weekend &lt;- data.frame(interval = unique(activity2\_weekend$interval),
avg = as.numeric(mean\_activity2\_weekend), day = rep("weekend",
length(mean\_activity2\_weekend))) df\_final &lt;- rbind(df\_weekday,
df\_weekend)

xyplot(avg ~ interval | day, data = df\_final, layout = c(1, 2), type =
"l", ylab = "Number of steps")
