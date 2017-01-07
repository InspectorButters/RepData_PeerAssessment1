# Activity Monitoring Data Analysis
Robert B. Herget  
January 6, 2017  




## Loading and preprocessing the data

1. Load the data (i.e. read.csv()).


```r
#Load the following libraries
library(plyr)
```

```
## Warning: package 'plyr' was built under R version 3.3.2
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)

#Setup project file
if(!file.exists("./Activity Monitoring")){
  dir.create("./Activity Monitoring")
}

#Download, extract & import data... 
#...this method requires you do know .csv filename beforehand
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
rm(temp)
```


2. Process/transform the data (if necessary) into a format suitable for your analysis.

```r
#No transformations necessary for the purposes of this analysis.
```



## What is the mean total number of steps taken per day?
Note: Missing values are ignored in this section of the assignment.

1. Calculate the total number of steps taken per day.

```r
totalSteps_day <- data %>%
    select(date, steps) %>%
    group_by(date) %>%
      summarize_each(funs(sum(., na.rm=TRUE)))
```

2. Make a histogram of the total number of steps taken each day.

```r
names(totalSteps_day)
```

```
## [1] "date"  "steps"
```

```r
with(totalSteps_day,
     hist(x = steps, col = "blue",
          xlab = "Total Steps per Day",
          breaks = 10,
          main = "Frequency of Total Steps/Day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day.

```r
with(totalSteps_day,
     c(mean = mean(steps, na.rm = TRUE),
       median = median(steps, na.rm = TRUE)))
```

```
##     mean   median 
##  9354.23 10395.00
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
avgActivity <- data %>%
    select(interval, steps) %>%
      group_by(interval) %>%
        summarise_each(funs(mean(., na.rm=TRUE)))

#rename columns to more accurately reflect data
names(avgActivity) <- c("interval", "Average_Steps")

#a plot to graphically depict interval/avg step data
with(avgActivity,
     plot(x = interval,
          y = Average_Steps,
          type = "l",
          xlab = "Interval (5 min)",
          main = "Average Number of Steps per 5-min Interval",
          col = "orange"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgActivity %>%
  filter(Average_Steps == max(Average_Steps))
```

```
## # A tibble: 1 × 2
##   interval Average_Steps
##      <int>         <dbl>
## 1      835      206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).

```r
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset.  The strategy does not need to be sophisticated.  For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
data2 <- data

impute <- function(x, fun) {
  missing <- is.na(x)
  replace(x, missing, fun(x[!missing]))
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# based on the "impute" function written above

data2 <- ddply(data2, ~ interval, transform, steps = impute(steps, mean))

totalSteps_day2 <- data2 %>%
  select(date, steps) %>%
  group_by(date) %>%
  summarize_each(funs(sum(., na.rm=TRUE)))
```

4. Make a histogram of the total number of steps taken each day, and calculate and report the mean and median total number of steps taken per day.  Do these values differ from the estimates from the first part of the assignment?  What is the impact of imputing the missing data on the estimates of the total daily number of steps?

```r
#make histogram
with(totalSteps_day2,
     hist(x = steps, col = "blue",
          xlab = "Total Steps per Day",
          breaks = 10,
          main = "Frequency of Total Steps/Day"))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#calculate mean and median
with(totalSteps_day2, c(mean = mean(steps, na.rm = FALSE), 
                        median = median(steps, na.rm = FALSE)))
```

```
##     mean   median 
## 10766.19 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
#note: this will overwrite previous "data2" variable
data2 <- mutate(data,
                dayType = as.factor(sapply(data$date,
                          FUN = function(date) {
                            day <- weekdays(as.Date(date))
                            if(day == "Saturday" | day == "Sunday") {
                              "weekend"
                            }
                            else {
                              "weekday"
                              }
                            })))
```

2. Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avgActivity2 <- data2 %>% 
    select(interval, steps, dayType) %>%
      group_by_(.dots = c("dayType", "interval")) %>%
        summarise_each(funs(mean(., na.rm = TRUE)))

xyplot(steps ~ interval | dayType,
       avgActivity2, type = "l",
       xlab = "Interval (5 min)",
       ylab = "Average_Steps",
       main = "Average Number of Steps per 5-min Interval",
       col = "orange")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
