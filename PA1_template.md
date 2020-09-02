---
title: "Reproducible Research - Week 2 Assignment"
author: "Christopher Heard"
date: "01/09/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Data Processing

First, the required packages are loaded and the working directory is set. Then, the data can be loaded into a new dataframe:
```{r data1}

library(dplyr)
library(ggplot2)
setwd("C:/users/chris/documents/r course/reproducible research")
data <- read.csv("activity.csv")

```

## Task 1

To process the data required for making the first histogram, the following dplyr code is used:

```{r task1}

dailysum <- data %>%
  group_by(date) %>%
  summarise(daily_total = sum(steps))

```

The following histogram is then constructed:

```{r plot1}

h1 <- ggplot(dailysum,aes(x=daily_total))
h1 + geom_histogram(binwidth = 1000)+
  labs(title="Total number of steps taken in a day") +
  xlab("Total number of steps")+
  ylab("Count")


```


```{r task1averages}

daily_mean <- signif(mean(dailysum$daily_total,na.rm = T),5)
daily_median <- signif(median(dailysum$daily_total, na.rm = T),5)

```

The mean number of steps is `r daily_mean`
The median number of steps is `r daily_median`

# Task 2

The second graph requires the grouping together of the 5 minute interval values. 

```{r task2}

step_int <- data %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps,na.rm=T))
  
```



```{r plot2}

h2 <- ggplot(step_int,aes(x=interval,y=avg_steps))
h2 + geom_line()+
    labs(title="Average number of steps taken per 5 minute interval") +
  xlab("Interval")+
  ylab("Step count")

```

# Task 3

It was decided that the NA values should be replaced with the mean values for their respective time intervals.

To achieve this, the vector for the average steps over all days was repeated for every unique date which occurred. This vector was then multiplied by the logical is.na vector for the step counts. This new vector (add) results in a zero if the number was recorded and an average value if the step count was not recorded.

The NA values were all converted to zeros and the add vector was added to the original step vector, thus replacing all of the NA values with mean values.


```{r task3}

nd<- data[,2] %>% unique() %>% length()

fill <- step_int[,2] %>% unlist() %>% rep(nd)

add <- fill * is.na(data[,1])

data_new <- data
data_new$steps[is.na(data_new$steps)] <- 0
data_new$steps = data_new$steps + add

total_steps <- data_new %>%
  group_by(date) %>%
  summarise(total=sum(steps))

```



```{r plot3}

h2 <- ggplot(total_steps,aes(x=total))
h2 + geom_histogram(binwidth = 1000)+
  labs(title="Total number of steps taken in a day with estimated NA values") +
  xlab("Total number of steps")+
  ylab("Count")

```

It is now easy to get the new mean and median values based on teh estimated step values.

```{r task3averages}

t3_med <- median(total_steps$total)
t3_mean <- mean(total_steps$total)

```

# Task 4

The following code adds a new column to the data table factoring whether the step counter is referring to a weekday or weekend:

```{r task4}

weekdays <- weekdays(as.Date(data_new$date))

day_type<-unlist(lapply(weekdays, function(x) if(x=="Saturday"|x=="Sunday")x<-"Weekend" else x<-"Weekday"))

data_new <- mutate(data_new,day_type = day_type)

```

The plots can then be split by day type to illustrate the differences between weekdays and weekends.

```{r plot4}

weekday_data <- data_new %>% 
  group_by(interval,day_type) %>% 
  summarise(mean=mean(steps))

plot4 <- ggplot(weekday_data,aes(x=interval,y=mean)) +
  geom_line() +
  facet_grid(day_type~.)+
  labs(title="Number of steps taken per 5 minute interval") +
  xlab("Interval")+
  ylab("Step count")
  print(plot4)

```
