library(dplyr)
library(lubridate)
library(ggplot2)

#import data
data<-read.csv("activity.csv")%>%
    mutate(date=ymd(date))


summary.by.day<-data%>%
    group_by(date)%>%
    summarise(total=sum(steps,na.rm=TRUE))
qplot(summary.by.day$total,binwidth=1000,xlab="Total steps/day",ylab = "Count")
mean(summary.by.day$total)
median(summary.by.day$total)

summary.by.interval<-data%>%
    group_by(interval)%>%
    summarise(mean=mean(steps,na.rm=TRUE))
qplot(interval,mean,data=summary.by.interval,geom="line",xlab="Interval",ylab="Average steps")
arrange(summary.by.interval,desc(mean))[[1,1]]

#missing values
sum(is.na(data$steps))

#impute values
imputed.data<-data%>%
    left_join(summary.by.interval)%>%
    mutate(imputed.steps=ifelse(is.na(steps), mean, steps))%>%
    select(interval,date,steps=imputed.steps)

imputed.summary.by.day <- imputed.data%>%
    group_by(date)%>%
    summarise(total=sum(steps))
qplot(imputed.summary.by.day$total,binwidth=1000,xlab="Total steps/day",ylab = "Count")
mean(imputed.summary.by.day$total)
median(imputed.summary.by.day$total)