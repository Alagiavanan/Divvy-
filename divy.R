#DIVVY BIKE SHARE ANALYSIS
#DPA PROJECT
#Akhil Suryadevara			            (A20391322)
#Ashwin kadammaje Giridhar     		  (A20399458)
#Azlagiavanan Senthil  			        (A20398151)               
#Poojitha Bangalore Srinivasan  		(A20405615)



rm(list=ls())
#list of libraries:
library(data.table)
library(plyr)
library(lubridate)
library(ggplot2)
library(graphics)
library(ggmap)
library(devtools)
library(forecast)


#Reading the dataset to a data table:
F1 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2017_Q1Q2/Divvy_Trips_2017_Q1.csv", header = TRUE)
View(F1)
F2 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2017_Q1Q2/Divvy_Trips_2017_Q2.csv", header = TRUE)
View(F2)
F3 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2017_Q3Q4/Divvy_Trips_2017_Q3.csv", header = TRUE)
View(F3)
F4 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2017_Q3Q4/Divvy_Trips_2017_Q4.csv", header = TRUE)
View(F4)
divvy_list <- list(F1,F2,F3,F4)
divvy <- ldply(divvy_list, data.table)
class(divvy)
View(divvy)

#Finding the structure of the object "divvy" using str and generating the summary for divvy.
str(divvy)
summary(divvy)

# creating a vector for numerical values from the dataset for future usage in the code.
numeric_list <- c('trip_id', 'bikeid', 'tripduration', 'from_station_id', 'to_station_id', 'birthyear')
for (i in numeric_list) {
  divvy[,i] = as.integer(divvy[,i])
}

#Pre-processing.Handling missing values.
#Variables gender and birthyear have missing values.
divvy$gender[divvy$gender==""] <- 'unknown'
boxplot(divvy$birthyear)
divvy$birthyear[is.na(divvy$birthyear)] <- 1985

#handling date-time using POSIXt Date-time Class.
divvy$start_time = as.POSIXlt(divvy$start_time,format="%m/%d/%Y %H:%M")
divvy$end_time = strptime(divvy$end_time, format = "%m/%d/%Y %H:%M")


divvy$usertype = as.factor(divvy$usertype)
divvy$gender = as.factor(divvy$gender)

divvy <- divvy[!(divvy$usertype=='Dependent'),]
divvy$usertype <- droplevels(divvy$usertype)

str(divvy)
summary(divvy)


divvy <- divvy[!(divvy$birthyear < 1940),]

plot(divvy$usertype)
plot(divvy$gender)

hist(divvy$birthyear)

boxplot(divvy$birthyear)

divvy_final <- divvy

View(divvy_final)
divvy_final$start_hour <- hour(divvy_final$start_time)
divvy_final$start_month <- month(divvy_final$start_time)
str(divvy_final)

hist(divvy_final$start_hour)
hist(divvy_final$start_month)

divvy_final$week <- weekdays(divvy_final$start_time)
divvy_final$week <- as.factor(divvy_final$week)

barplot(sort(prop.table(table(divvy_final$week)), decreasing = TRUE))

plot(divvy_final$week, divvy_final$start_month)

divvy_subscriber <- divvy_final[divvy_final$usertype=='Subscriber',]
class(divvy_subscriber)

divvy_customer <- divvy_final[divvy_final$usertype=='Customer',]  
class(divvy_customer)  

str(divvy_subscriber)
summary(divvy_subscriber)
View(divvy_subscriber)

str(divvy_customer)
summary(divvy_customer)
View(divvy_customer)

par(mfrow= c(1,2))
barplot(sort(prop.table(table(divvy_subscriber$week)), decreasing = TRUE), main = 'Subscriber', cex.names = 0.7)
barplot(sort(prop.table(table(divvy_customer$week)), decreasing = TRUE), main = 'Customer', cex.names = 0.7)



par(mfrow = c(1,1))

str(as.factor(divvy_final$from_station_id))

barplot(sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:10], main = 'Frequently utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
best_station_id <- sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:10]
names(best_station_id)

best_station <- c()
for(i in names(best_station_id))
{
  best_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(best_station)

barplot(sort(table(as.factor(divvy_final$from_station_id)))[1:10], main = 'Least utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
least_station_id <- sort(table(as.factor(divvy_final$from_station_id)))[1:10]
names(least_station_id)

least_station <- c()
for(i in names(least_station_id))
{
  least_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(least_station)

location <- fread("/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2017_Q3Q4/Divvy_Stations_2017_Q3Q4.csv", header = TRUE)
View(location)
location$name <- NULL
location$online_date = NULL
divvy_final <- cbind(divvy_final, location[match(divvy_final$from_station_id, location$id),][,2:4])


point1 <- divvy_final[match(names(best_station_id), divvy_final$from_station_id),][,16:18]
point2 <- divvy_final[match(names(least_station_id), divvy_final$from_station_id),][,16:18]
points <- rbind(point1, point2)
points$class <- c(rep('Most Used',10), rep('Least Used',10))
View(points)
map <- get_map(location = 'chicago', zoom = 10)
ggmap(map) + geom_point(data = points, aes( longitude,latitude, color=factor(class)), size = 3)


table(divvy_final$start_month)

day_wise <- table(as.Date(divvy_final$start_time))
class(day_wise)
as.data.frame(day_wise)
View(day_wise)
plot(day_wise)

barplot(sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:20], main = 'Frequently utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
best_station_id <- sort(table(as.factor(divvy_final$from_station_id)), decreasing = TRUE)[1:20]
names(best_station_id)

best_station <- c()
for(i in names(best_station_id))
{
  best_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(best_station)

barplot(sort(table(as.factor(divvy_final$from_station_id)))[1:20], main = 'Least utilized station', xlab = "Station ID", ylab = 'No of users', col = 'lightblue')
least_station_id <- sort(table(as.factor(divvy_final$from_station_id)))[1:20]
names(least_station_id)

least_station <- c()
for(i in names(least_station_id))
{
  least_station[i] = divvy_final$from_station_name[divvy_final$from_station_id==i][1]
}
print(least_station)

#Geographic view
points1 <- divvy_final[match(names(best_station_id), divvy_final$from_station_id),][,16:18]
points2 <- divvy_final[match(names(least_station_id), divvy_final$from_station_id),][,16:18]

fullpoints <- rbind(points1, points2)
fullpoints$class <- c(rep('Most Used',20), rep('Least Used',20))
View(fullpoints)

map <- get_map(location = 'chicago', zoom = 11)
ggmap(map) + geom_point(data = fullpoints, aes(longitude, latitude, color=factor(class)), size = 3)


#Time series Analysis:

F5 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_Q1.csv", header = TRUE)
View(F5)
F6 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_04.csv", header = TRUE)
View(F6)
F7 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_05.csv", header = TRUE)
View(F7)
F8 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q1Q2/Divvy_Trips_2016_06.csv", header = TRUE)
View(F8)
F9 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q3Q4/Divvy_Trips_2016_Q3.csv", header = TRUE)
View(F9)
F10 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2016_Q3Q4/Divvy_Trips_2016_Q4.csv", header = TRUE)
View(F10)
F11 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015-Q1Q2/Divvy_Trips_2015_Q1.csv", header = TRUE)
View(F11)
F12 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015-Q1Q2/Divvy_Trips_2015_Q2.csv", header = TRUE)
View(F12)
F13 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_Q4.csv", header = TRUE)
View(F13)
F14 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_Q7.csv", header = TRUE)
View(F14)
F15 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_Q8.csv", header = TRUE)
View(F15)
F16 <- fread(file = "/Users/alagu/Desktop/Divvy dataset/Divvy_Trips_2015_Q3Q4/Divvy_Trips_2015_Q9.csv", header = TRUE)
View(F16)

divvy_list <- list(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16)


divvy <- ldply(divvy_list, data.table)
class(divvy)
View(divvy)

str(divvy)

numeric_list <- c('trip_id', 'bikeid', 'tripduration', 'from_station_id', 'to_station_id', 'birthyear')
for (i in numeric_list) {
  divvy[,i] = as.integer(divvy[,i])
}

divvy$gender[divvy$gender==""] <- 'unknown'
divvy$birthyear[is.na(divvy$birthyear)] <- 1985

divvy$starttime = as.POSIXlt(divvy$starttime,format="%m/%d/%Y %H:%M")
divvy$stoptime = strptime(divvy$stoptime, format = "%m/%d/%Y %H:%M")

divvy$usertype = as.factor(divvy$usertype)
divvy$gender = as.factor(divvy$gender)

summary(divvy)

divvy <- divvy[!(divvy$usertype=='Dependent'),]
divvy$usertype <- droplevels(divvy$usertype)

str(divvy)
summary(divvy)

divvy <- divvy[!(divvy$birthyear < 1940),]

divvy_time <- as.data.frame(table(as.Date(divvy$starttime)))
View(divvy_time)
plot(divvy_time)

#Built in Time series analysis Function(Spectral analysis):
time_series <- ts(divvy_time$Freq, frequency = 365)
time_series
plot(time_series)

#Decomposing the obtained time series:
time_dec <- decompose(time_series)
plot(time_dec$seasonal)
plot(time_dec$random)
plot(time_dec$trend)
plot(time_dec$figure)
plot(time_dec)

#finding autocorrelation function and partial autocorrelation function for the built in model:
length(time_series)
par(mfrow=c(2,2))
sapply(1:4, function(x)plot(time_series[-c(731:(731-x+1))],time_series[-c(1:x)]))
par(mfrow=c(2,2))
sapply(7:10, function(x)plot(time_series[-c(731:(731-x+1))],time_series[-c(1:x)]))
acf(time_series)
pacf(time_series)

#ARIMA model and Forecasting:
fit <- arima(time_series, order = c(1,2,7))
tsdisplay(residuals(fit))
summary(fit)
AIC(fit)
fcast <- forecast(fit, h=30)
plot(fcast)
summary(fcast)
BIC(fit)



