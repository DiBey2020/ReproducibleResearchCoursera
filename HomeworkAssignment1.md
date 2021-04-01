
---
title: "Assignment 1"
output:
  html_document: default
  pdf_document: default
---
  
First, we read in all the libraries we need for the project: knite, tidyverse, dbplyr, ggplot2, scales as follows:



```r
library(knitr)
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(scales)
```



The code for reading in the dataset and/or processing the data:

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
### Histogram of the total number of steps taken each day:


```r
total_steps <- count(data_activity, date, wt=steps)
total_steps$date <- as.Date(total_steps$date)
ggplot(total_steps, aes(n)) + geom_histogram(bins=61)+ xlab ( "Total number of steps on that day") + ylab("Count")
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24-1.png)


### Mean and median number of steps taken each day


```r
summary(total_steps$n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```


### Time series plot of the average number of steps taken

```r
interval_steps <- data_activity %>% group_by(interval)
interval_steps2 <- interval_steps %>% summarise(steps=mean(steps, na.rm=TRUE), na.rm=TRUE)
plot(type="l", interval_steps2$interval, interval_steps2$steps, xlab="Interval", ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26-1.png)

We calculate the interval in which the maximum number of steps were recorded via:

```r
int <- interval_steps2[interval_steps2$steps==max(interval_steps2$steps),]
int
```

The maximum number of steps is reached at the 835th interval of steps taken on average for all days recorded.

### Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):

```r
sum(is.na(data_activity$steps))
```

```
## [1] 2304
```

```r
sum(is.na(data_activity$interval))
```

```
## [1] 0
```

```r
sum(is.na(data_activity$date))
```

```
## [1] 0
```
The total number of NAs in the dataset is 2304 and it stems from the step column alone.


1. We impute the NAs with the mean number of steps of that interval. We first group the data by interval
2. We determine the mask of indices for rows with missing values in order to select and replace those rows with the mean number of steps for the corresponding interval
3. We assign the average of the interval to the missing values for that day
4. We merge the original data frame without the missing values with the data frame which we created in steps 1-4. 


We calculate the averages per interval for later imputing the missing values

```r
grouped_subset <- data_activity %>% group_by(interval)
grouped_subset2 <- grouped_subset %>% summarise(steps=mean(steps, na.rm=TRUE))
averages_per_interval <- grouped_subset2
```

The average number of steps for each interval are:

```r
head(averages_per_interval)
```

The following code chunk puts out the index values of the missing values


```r
indices <- which(is.na(data_activity$steps))
```

The ordering of the data_activity data frame is steps, date, interval. By indexing with the "indices" variable we get all the rows where missing data was recorded. In the following, we replace the NAs in these rows with the mean value of steps for this interval.

```r
NA_Values <- data_activity[indices,,]
head(data_activity[indices,,])
```

Merge the dataframe with the interval averages with the data_activity data frame 

```r
NA_Values_grouped <- NA_Values %>% group_by(interval)
result_NA_removed <- merge(NA_Values_grouped, averages_per_interval, by="interval")
head(result_NA_removed)
```
We replace the original steps column which still contains the NA values and we order the dataframe via the date in order to perform the full join of the rows where the NAs have been replaced and the dataframe from which the NA rows have been substracted - after the join, we have the full data frame without NAs and with the mean value of steps of the respective interval in place of the original NAs. 

```r
result_NA_removed$steps.x <- NULL
result_NA_removed <- arrange(result_NA_removed,date)
```
One step in between is renaming the columns from steps.y to steps, converting the datatype of the steps column to integer (as the mean values of the steps for each interval is numeric) and reordering the columns in order to facilitate the join. 

```r
df <- result_NA_removed[,c(3,2,1)]
df <- rename(df, steps = steps.y )
df <- transform(df, steps = as.integer(steps))
df2 <- data_activity[-indices,,]
df4 <- full_join(df,df2)
```

```
## Joining, by = c("steps", "date", "interval")
```
We calculate the total number of steps for each date with this new data frame where the NAs are imputet. 

```r
total_steps_withoutNA <- count(df4, date, wt=steps)
summary(total_steps_withoutNA$n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10641   10750   12811   21194
```
We compare this with the original data frame which still contains the NAs:

```r
summary(total_steps$n)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```

```r
total_steps_withoutNA$date <- as.Date(total_steps$date)
ggplot(total_steps_withoutNA, aes(n)) + geom_histogram(bins=61)+ xlab ( "Total number of steps - with NA imputed - on that day") + ylab("Count")
```

![plot of chunk unnamed-chunk-40](figure/unnamed-chunk-40-1.png)

We notice that the median and mean increase after imputing. 

### Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
df4$date <- as.Date(df4$date)
df4$weekdays <- weekdays(df4$date)

weekenddays = c("Samstag", "Sonntag")
df4_wdays <- factor(df4$weekdays %in% weekenddays, levels=c("TRUE", "FALSE"), labels=c("weekend","weekdays"))
df4$factor_we <- df4_wdays


df4_stepsmean <- df4 %>% group_by(interval, factor_we)
df4_stepsmean2 <- df4_stepsmean %>% summarise(steps=mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
qplot(interval, steps, data=df4_stepsmean2, facets= factor_we~.) + geom_line()+ xlab("Interval") + ylab("Number of Steps")
```

![plot of chunk unnamed-chunk-41](figure/unnamed-chunk-41-1.png)

```r
knit2html("HomeworkAssignment1.Rmd")
```

```
## 
## 
## processing file: HomeworkAssignment1.Rmd
```

```
##   |                                                                        |                                                                |   0%  |                                                                        |..                                                              |   3%
##   ordinary text without R code
## 
##   |                                                                        |...                                                             |   5%
## label: unnamed-chunk-43
##   |                                                                        |.....                                                           |   8%
##   ordinary text without R code
## 
##   |                                                                        |.......                                                         |  11%
## label: unnamed-chunk-44 (with options) 
## List of 1
##  $ echo: logi FALSE
## 
##   |                                                                        |........                                                        |  13%
##   ordinary text without R code
## 
##   |                                                                        |..........                                                      |  16%
## label: unnamed-chunk-45
```

```
##   |                                                                        |............                                                    |  18%
##   ordinary text without R code
## 
##   |                                                                        |.............                                                   |  21%
## label: unnamed-chunk-46
##   |                                                                        |...............                                                 |  24%
##   ordinary text without R code
## 
##   |                                                                        |.................                                               |  26%
## label: unnamed-chunk-47
```

```
##   |                                                                        |...................                                             |  29%
##   ordinary text without R code
## 
##   |                                                                        |....................                                            |  32%
## label: unnamed-chunk-48
##   |                                                                        |......................                                          |  34%
##   ordinary text without R code
## 
##   |                                                                        |........................                                        |  37%
## label: unnamed-chunk-49
##   |                                                                        |.........................                                       |  39%
## label: unnamed-chunk-50
##   |                                                                        |...........................                                     |  42%
## label: unnamed-chunk-51
##   |                                                                        |.............................                                   |  45%
##   ordinary text without R code
## 
##   |                                                                        |..............................                                  |  47%
## label: unnamed-chunk-52
##   |                                                                        |................................                                |  50%
##   ordinary text without R code
## 
##   |                                                                        |..................................                              |  53%
## label: unnamed-chunk-53
##   |                                                                        |...................................                             |  55%
##   ordinary text without R code
## 
##   |                                                                        |.....................................                           |  58%
## label: unnamed-chunk-54
##   |                                                                        |.......................................                         |  61%
##   ordinary text without R code
## 
##   |                                                                        |........................................                        |  63%
## label: unnamed-chunk-55
##   |                                                                        |..........................................                      |  66%
##   ordinary text without R code
## 
##   |                                                                        |............................................                    |  68%
## label: unnamed-chunk-56
##   |                                                                        |.............................................                   |  71%
##   ordinary text without R code
## 
##   |                                                                        |...............................................                 |  74%
## label: unnamed-chunk-57
##   |                                                                        |.................................................               |  76%
##   ordinary text without R code
## 
##   |                                                                        |...................................................             |  79%
## label: unnamed-chunk-58
##   |                                                                        |....................................................            |  82%
##   ordinary text without R code
## 
##   |                                                                        |......................................................          |  84%
## label: unnamed-chunk-59
##   |                                                                        |........................................................        |  87%
##   ordinary text without R code
## 
##   |                                                                        |.........................................................       |  89%
## label: unnamed-chunk-60
##   |                                                                        |...........................................................     |  92%
## label: unnamed-chunk-61
```

```
##   |                                                                        |.............................................................   |  95%
##   ordinary text without R code
## 
##   |                                                                        |..............................................................  |  97%
## label: unnamed-chunk-62
```

```
##   |                                                                        |................................................................| 100%
## label: unnamed-chunk-63
```

```
## output file: HomeworkAssignment1.md
```
