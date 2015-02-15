require(plyr)
data <- read.csv("activity.csv")
cleanData<-data[complete.cases(data),]
cdata <- ddply(cleanData, c("date"), summarise,sum    = sum(steps))
hist(cdata$sum)


#calculate the mean
mean<-mean(cdata$sum)

#calculate the median
median<-median(cdata$sum)


#Daily pattern
stepsPerInterval <- ddply(cleanData, c("interval"), summarise,average    = mean(steps))
plot(stepsPerInterval$interval,stepsPerInterval$average,type="l",ylab="Average Steps Taken",xlab="Interval")


#Inputting missing values

#total number of erronous observations
nErrObs=nrow(data)-nrow(cleanData)


#Substitute for teh NAs for the average steps in that interval
dataSub<-data
for (i in 1:nrow(data)) {
  #if teh value is a NA find the average number in the interval and substitute it
  if(is.na(data$steps[i])){
    intervalI<-data[i,"interval"]
    meanIntervalI<-mean(stepsPerInterval[stepsPerInterval$interval==intervalI,"average"])
    dataSub[i,"steps"]<-meanIntervalI
  }
}

cdataSub <- ddply(dataSub, c("date"), summarise,sum    = sum(steps))
hist(cdataSub$sum)

#calculate the mean
meanSub<-mean(cdataSub$sum)

#calculate the median
medianSub<-median(cdataSub$sum)



#weekendpattern
dataSub$date=as.POSIXlt(dataSub$date)
dataSub$dayOfWeek=weekdays(dataSub$date)
dataSub$weekends = factor(weekdays(dataSub$date) %in% c("Sunday","Saturday"), labels = c("weekday","weekend"))

aggregatedDataWeekday <- ddply(dataSub[dataSub$weekends=="weekday",], c("interval"), summarise,average    = mean(steps))
aggregatedDataWeekend <- ddply(dataSub[dataSub$weekends=="weekend",], c("interval"), summarise,average    = mean(steps))



par(mfrow = c(2,1))
#first subplot
plot(aggregatedDataWeekday$interval,aggregatedDataWeekday$average,type="l",main="Weekday",ylab="Number of Steps",xlab="Interval")
#second subplot
plot(aggregatedDataWeekend$interval,aggregatedDataWeekend$average,type="l",main="Weekend",ylab="Number of Steps",xlab="Interval")














