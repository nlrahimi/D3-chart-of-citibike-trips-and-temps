citibike_all <- read.csv("all-data.csv", header = TRUE, sep = ",")
cb2  <- read.csv("all-data.csv", header = TRUE, sep = ",")
# Multiple Linear Regression Example 
library("ggplot2") # load ggplot2
citibike_all$norainind<-citibike_all$precip==0 | citibike_all$precip=="T"
citibike_all$nocloudind<-citibike_all$clouds<=2 | citibike_all$clouds=="T"

citibike_all$miles <- round(citibike_all$Miles.traveled.today..midnight.to.11.59.pm., digits=0)

fit1 <- lm(miles ~ day_of_week + maxtemp + precip + clouds + Min.DewpointF + maxwind, data=citibike_all)
summary(fit1) # show results
anova(fit1) # anova table 

citibike_all$fit <- as.numeric(fitted(fit1))
plot(citibike_all$fit/1000,citibike_all$miles/1000,xlim=c(5, 100), ylim=c(5, 100),xlab="Predicted miles in thousands", ylab="Actual miles in thousands",main="Model of daily miles travelled",col="blue")
abline(0,1)

a1 <- citibike_all

a1$Events <- NULL
a1$Date <- NULL
a1$day_of_week <- NULL
a1$nocloudind <- NULL
a1$norainind <- NULL
a1$year <- NULL
a1$month <- NULL
a1$day <- NULL

allTrips <- sum(a1$trips)
allMiles <- sum(a1$Miles.traveled.today..midnight.to.11.59.pm.)
avgMilesPerTrip <- allMiles/allTrips

  data.matrix(a1, rownames.force = NA)
# correlations
chisq.test(abs(a1))
cor(a1)
install.packages("corrgram")
library(corrgram)
corrgram(a1,upper.panel=panel.pie,lower.panel=panel.ellipse)
cor(a1$miles,a1$maxtemp,method="pearson")
fit2 <- lm(miles ~ meantemp, data=citibike_all)
summary(fit2)

cor(a1$miles,a1$maxwind,method="pearson")
cor(citibike_all$miles,citibike_all$maxwind,method="pearson")
cor(citibike_all$miles,citibike_all$clouds,method="pearson")
cor(citibike_all$miles,citibike_all$precip,method="pearson")

plot(citibike_all$miles/1000,citibike_all$meantemp,xlab="Daily miles traveled in thousands", ylab="Daily mean temp",main="Miles per day by mean temperature", col="blue")


boxplot(citibike_all$miles/1000~citibike_all$day_of_week,col="blue",main="Boxplot by day of week")

plot(a1$members,ylab="Annual members",xlab="Days since launch",main="Total Annual Memberships Bought", col="blue")
plot(a1$trips,ylab="Daily Trips",xlab="Days since launch",main="Daily Trips", col="red")
plot(a1$Miles.traveled.today..midnight.to.11.59.pm.,ylab="Daily Miles Traveled",xlab="Days since launch",main="Daily Miles Traveled", col="navyblue")
plot(a1$dailytrips_permember,ylab="Daily Trips per Annual Member",xlab="Days since launch",main="Daily Trips per Annual Member", col="red4")
plot(a1$meantemp,ylab="Daily Mean Temperature",xlab="Days since launch",main="Daily Mean Temperature", col="violetred4")

allTrips <- sum(a1$trips)
allDailyPasses <- sum(a1$X24.Hour.Passes.Purchased..midnight.to.11.59.pm.)
allSevenDayPasses <- sum(a1$X7.Day.Passes.Purchased..midnight.to.11.59.pm.)

(allTrips-(1.5*allDailyPasses+5*allSevenDayPasses))/allTrips

plot(a1$trips,a1$meantemp)

plot(a1$X24.Hour.Passes.Purchased..midnight.to.11.59.pm.)
plot(a1$X7.Day.Passes.Purchased..midnight.to.11.59.pm.)
plot(a1$meantemp,a1$dailytrips_permember*1000)
abline(lsfit(a1$meantemp,a1$dailytrips_permember*1000))
par(mfrow=c(2,2))


boxplot(citibike_all$miles/1000~cut(citibike_all$meantemp,breaks=(c(2:10)*10)), ylab="Daily miles traveled in thousands", xlab="Daily mean temp in degrees F",main="Miles per day by mean temperature", col="blue")
boxplot(citibike_all$miles/1000~cut(citibike_all$maxtemp,breaks=(c(2:10)*10)), ylab="Daily miles traveled in thousands", xlab="Daily max temp in degrees F",main="Miles per day by max temperature", col="blue")
boxplot(citibike_all$miles/1000~cut(citibike_all$mintemp,breaks=(c(2:10)*10)), ylab="Daily miles traveled in thousands", xlab="Daily min temp in degrees F",main="Miles per day by min temperature", col="blue")
boxplot(citibike_all$miles/1000~cut(citibike_all$Min.DewpointF,breaks=(10)), ylab="Daily miles traveled in thousands", xlab="Daily min dew point",main="Miles per day by min dew point", col="blue")

par(mfrow=c(1,1))
boxplot(citibike_all$miles/1000~relevel(cb2$day_of_week, ref="Wednesday"),ylab="Daily miles traveled in thousands", xlab="Day of week",main="Miles per day by day of week", col="red")
boxplot(citibike_all$miles/1000~cut(citibike_all$maxwind,breaks=(9)),ylab="Daily miles traveled in thousands", xlab="Daily max wind speed in mph",main="Miles per day by max wind speed", col="purple")
boxplot(citibike_all$miles/1000~cut(citibike_all$clouds,breaks=(9)),ylab="Daily miles traveled in thousands", xlab="Daily cloud cover",main="Miles per day by cloud cover", col="orange")
boxplot(citibike_all$miles/1000~cut(citibike_all$precip,breaks=(15)),ylab="Daily miles traveled in thousands", xlab="Daily cloud cover",main="Miles per day by cloud cover", col="orange")
plot(citibike_all$precip,citibike_all$miles/1000, ylab="Daily miles traveled in thousands", xlab="Daily precipitation in inches",main="Miles per day by precipitation", col="blue")


# simple ggplot
ggplot(citibike_all,aes(miles,fit)) + geom_point(aes(colour=day_of_week))
# Other useful functions 
#coefficients(fit) # model coefficients
confint(fit1, level=0.95) # CIs for model parameters 
plot(citibike_all$miles,fitted(fit)) # predicted values
a1$residuals <- residuals(fit1) # residuals
boxplot(a1$residuals~citibike_all$month)
#vcov(fit) # covariance matrix for model parameters 
#influence(fit) # regression diagnosticsplot