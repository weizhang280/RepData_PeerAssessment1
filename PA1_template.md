---
title: "Reproducible Research: Peer Assessment 1"
author: "Wei Zhang"
date: "6/17/2020"
output: 
  html_document: 
    keep_md: yes
---


&nbsp;  

#### 1. Code for reading in the dataset and/or processing the data

```r
df <- read.csv("activity.csv", header = TRUE, sep=",", 
                     colClasses = c("integer", "Date", "integer"))
```

&nbsp;  

#### 2. Histogram of the total number of steps taken each day

```r
dailySum <- df %>% na.omit(df) %>% group_by(date) %>% summarize(total = sum(steps))
g <- ggplot(dailySum)         
g + geom_histogram(aes(x=date, weight=total), binwidth = 1, col="white",fill="grey30") +
        labs(x="Date", y="Total Number of Steps", title = "Daily Total Number of Steps") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_date(date_labels = "%m/%d", date_breaks = "3 days", date_minor_breaks = "1 day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

&nbsp;  

#### 3. Mean and median number of steps taken each day

```r
summary(dailySum$total)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```
<span style="color: red;">Mean: 10766 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
Median: 10765</span>

&nbsp; 

#### 4. Time series plot of the average number of steps taken

```r
## calc mean group-by interval
mean5Min <- df %>% na.omit(df) %>% group_by(interval) %>% summarize(mean = mean(steps))

## add 5-min time series column: intervalTime
timeSequence <- seq(as.POSIXlt("1970-01-01 00:00:00", tz="GMT"), 
                    as.POSIXlt("1970-01-01 23:55:00", tz="GMT"), by = "5 min")
mean5Min$intervalTime <- timeSequence

##Time series plot
g <- ggplot(mean5Min, aes(x=intervalTime, y=mean)) 
g + geom_line(col = "red") +
        labs(x="Time Line: 5-Minute Interval", y="Average Number of Steps", 
             title="Average Number of Steps Per 5 minutes") +
        scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour", 
                     date_minor_breaks = "5 min") +
        theme(plot.title = element_text(hjust = 0.5), panel.background = element_rect(fill = "grey"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

&nbsp; 

#### 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
x <- format(mean5Min[[which.max(mean5Min$mean),3]], "%H:%M")
```
<span style="color: red;">08:35 interval, on average, contains the maximum number of steps across all days.</span>

&nbsp;  

#### 6. Code to describe and show a strategy for imputing missing data

```r
s <- sum(is.na(df$steps))
m <- percent(mean(is.na(df$steps)))
```
<span style="color: red;">Total number of the missing values is 2304, it is 13% of the activity dataset</span>    
&nbsp;  
**Mean Imputing Method:**        
filling in all of the missing values with the mean for that 5-minute interval

```r
## generate NA subset by inner join isNA subset of main dataframe df with mean5Min dataframe
naSubset<- inner_join(x = df[is.na(df$steps), ], y = mean5Min, by = "interval")

##Create a new dataset that is equal to the original dataset but with the missing data filled in.
newDf <- df
newDf$steps[is.na(df$steps)] <- naSubset$mean
```

&nbsp; 

#### 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
newDailySum <- newDf %>% group_by(date) %>% 
        summarize(newTotal = sum(steps))

ggplot(newDailySum) + 
        geom_histogram(aes(x=date, weight=newTotal), binwidth = 1,
                       col="white",fill="grey30") +
        labs(x="Date", y="Total Number of Steps", 
             title = "Daily Total Steps with NA filled") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_date(date_labels = "%m/%d", date_breaks = "3 days", date_minor_breaks = "1 day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

**The impact of Mean Imputing Method:**  

```r
compare <- rbind(summary(dailySum$total), summary(newDailySum$newTotal))
compare <- cbind(compare, "std" = c(sd(dailySum$total), sd(newDailySum$newTotal)))
row.names(compare) <- c("Original", "Imputed")
compare
```

```
##          Min. 1st Qu.   Median     Mean 3rd Qu.  Max.      std
## Original   41    8841 10765.00 10766.19   13294 21194 4269.180
## Imputed    41    9819 10766.19 10766.19   12811 21194 3974.391
```
<span style="color: red;">Mean and original datset size are both perserved by Mean Imputing Method. Median has a slight change.  Standard Deviation has decreased since the variability in the data is reduced.

&nbsp;  

#### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
## 1. Create a new factor variable in the dataset with two levels – 
##“weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend <- c("Saturday", "Sunday")

newDf$w_factor <-weekdays(newDf$date)
newDf$w_factor[newDf$w_factor %in% weekday] <- "weekday"
newDf$w_factor[newDf$w_factor %in% weekend] <- "weekend"

##Make a panel plot containing a time series plot (i.e. type = "l") 
##of the 5-minute interval (x-axis) and the average number of steps taken, 
##averaged across all weekday days or weekend days (y-axis). 

meanDf <- newDf %>% group_by(w_factor, interval) %>% summarize(wMean = mean(steps))

## add interval time sequence to weekdays and weekend
meanDf$intervalTime <- rep(timeSequence, 2)

g <- ggplot(meanDf, aes(x = intervalTime, y = wMean)) 
g + geom_line(col="blue") +
        facet_wrap(~w_factor, ncol=1)  +
        labs(x = "Time Line: 5-Minute Interval", y = "Number of Steps", title = "Average Number of Steps Per 5 Minutes", subtitle = "Weekday and Weekend Comparison") +
        scale_x_datetime(date_labels = "%H:%M", date_breaks = "1 hour", 
                         date_minor_breaks = "5 min") +
        theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
        theme(strip.background =element_rect(fill="wheat"))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
