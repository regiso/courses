Introduction

It is now possible to collect a large amount of data about personal
managment using activity monitoring devices like Fitbit, Nike Fuelband
or Jawbone Up. These devices ae part of the "quantified self" movement -
a group of enthusiasts who measure themeselves regularly to improve
health, find patterns in behavior, or because they are tech geeks. But
these data remain under-utilized because the raw data are hard to obtain
and there is a lack of statistical methods and software for process and
interpreting the data.

This assignment make use of the data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of 2 months of data from an anonymous individual
collected during October and November, 2012. It includes the number of
steps taken in 5 minute intervals each day.

The data comes from Coursera, Reproducible Research, Week 2 Project
Assignment. The code that follows explores these key issues:

1.  The mean total number of steps taken per day
2.  The average daily activity pattern
3.  The imputation of missing values
4.  Identifying differences between activity patterns on weekdays vs.
    weekends.

### 1. The mean total number of steps taken per day, histogram and means

#### a. The load\_and\_process chunk will load the "activity.csv" file from the

#### working directory and install packages for later analysis.

    setwd("C:/Users/AnalyticsRMO/Desktop/Coursera/Reproducible Research/Reproducible Research - P1 W2/Data")
    data <- read.csv("activity.csv")
    library(tidyr)

    ## Warning: package 'tidyr' was built under R version 3.3.1

    library(plyr)
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.3.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(lattice)
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.3.1

#### b. The raw\_steps\_per\_day chunk calculates total steps per day, creates a

#### histogram,and calcuates mean and median.

    steps_per_day <- data %>% select(steps:date) %>%group_by(date)%>%
      summarize(totsteps = sum(steps))
    hist(steps_per_day$totsteps, xlab="Total Steps per Day, with NAs")

![](PA1-template_files/figure-markdown_strict/raw_steps_per_day-1.png)

    mean_spd <- round(mean(steps_per_day$totsteps, na.rm=TRUE), digits = 1)
    median_spd <- median(steps_per_day$totsteps, na.rm=TRUE)

#### c. Between October and November 2012, mean total steps per day for this subject

#### is 1.0766210^{4}.The median is 10765.

### 2. The average daily activity pattern - plot average steps by interval, find max

#### a. The time\_series chunk converts interval to a factor, removes NAs, calculates

#### average steps per day by interval in order to create a plot of average steps

#### by interval and identify the interval containing the maximum average steps.

    data$intervalF <- as.factor(data$interval)
    timeseries <- data %>% filter(!is.na(steps)) %>%group_by(intervalF)%>% summarise(mean_spi=mean(steps))
    xyplot(timeseries$mean_spi ~ timeseries$intervalF, type="l")

![](PA1-template_files/figure-markdown_strict/time_series-1.png)

    maxsteps<- max(timeseries$mean_spi)
    maxinterval <- filter(timeseries, mean_spi==maxsteps)

#### b. Between October and November 2012, the 5 minute interval with the maximum

#### average total steps per day for this subject is 835.

### 3. Impute missing values - \# of NAs, compare & contrast mean & median

#### a. The NA count chunk calculates & reports the \# of NA's in the raw data

    NAcount <- filter(data, is.na(steps))
    NAs <- length(NAcount$steps)

#### The total number of missing values in the data is 2304.

#### b. The impute\_and\_plot chunk imputes missing values as the interval average,

#### creates a new dataset with imputed values, calculates & reports mean & median,

#### then compares these to the original data set to assess the impact of imputing.

    data1 <- merge(data, timeseries, by="intervalF") 
    data2 <- select(data1, -steps) 
    data3 <- cbind(data2, data$steps)
    data4 <- filter(data3, is.na(data$steps)) 
    data5 <- select(data4, intervalF:mean_spi) 
    data6 <- mutate(data5, steps=mean_spi) 
    data7 <- select(data6, -mean_spi) 

    data8 <- filter(data3, !is.na(data$steps)) 
    data9 <- select(data8, -mean_spi) 
    data10 <- select(data8, intervalF:interval, steps=starts_with("data"))

    data11 <- rbind(data10, data7) 

    steps_per_dayi <- data11 %>% select(steps:date) %>%group_by(date)%>%
      summarize(totsteps = sum(steps))
    hist(steps_per_dayi$totsteps, 
         xlab="Total Steps per Day, Interval Average imputed for NA's")

![](PA1-template_files/figure-markdown_strict/impute_and_plot-1.png)

    mean_spdi <- round(mean(steps_per_dayi$totsteps, na.rm=TRUE), digits=1)
    median_spdi <- median(steps_per_dayi$totsteps, na.rm=TRUE)

    summary_stats <- data.frame(NA_treatment = c("removed", "imputed"),
                                mean=c(mean_spd, mean_spdi),
                                median=c(median_spd, median_spdi))
    print(summary_stats)

    ##   NA_treatment    mean  median
    ## 1      removed 10766.2 10765.0
    ## 2      imputed 10006.1 10003.7

    resultsm <- if(mean_spd>mean_spdi) {
      "lower than"} else if(mean_spd<mean_spdi) {
        "higher than"} else if(mean_spd==mean_spdi) {
          "the same as"
        } 

    resultsmed <- if(median_spd>median_spdi) {
      "lower than"} else if(median_spd<median_spdi) {
        "higher than"} else if(median_spd==median_spdi) {
          "the same as"
        }            

Using imputed values delivers a mean that is lower than the mean
caculated by removing NA's. Imputed values also deliver a median that is
lower than the median observed in the data set with the NA's removed.
The imputed mean is 1.0006110^{4} - lower than the mean of dataset with
the NA's removed - 1.0766210^{4}. The imputed median - 1.000369810^{4} -
is lower than the median of the data set with the NA's eliminated -
1.000369810^{4}.

### 4. Differences in activities between weekdays and weekends

#### a. The weekday\_vs\_weekend chunk creates a new factor for weekend vs. weekday

#### & makes a panel plot with lattice to compare average steps across 5 minute intervals.

    data11$weekday <- as.factor(weekdays(as.Date(data11$date)))
    data12 <- data11 %>% filter(weekday=="Saturday"|weekday=="Sunday")%>%
      group_by(intervalF)%>%summarize(avg_spd=mean(steps))%>%mutate(weekpart="weekend")
    data13 <- data11 %>% filter(weekday=="Monday"|weekday=="Tuesday"|
      weekday=="Wednesday"| weekday=="Thursday"|weekday=="Friday")%>%
      group_by(intervalF)%>%summarize(avg_spd=mean(steps))%>%mutate(weekpart="weekday")
    data14 <- rbind(data12,data13)

    xyplot(avg_spd ~ intervalF|weekpart, data=data14, type="l", layout=c(1,2),
           ylab="Average Steps per Interval, NA's imputed",
           xlab="5 minute interval", main="Mean steps per inteval, 10/2012 - 11/2012")

![](PA1-template_files/figure-markdown_strict/weekday_vs_weekend-1.png)
