library(ggplot2) 
library(readr) 
library(fpp2)
library(urca)
library(fpp)

bfproject <- read.csv("C:/Users/Ruchi/Desktop/Ruchi/Rutgers/BusinessForecasting/project/GlobalLandTemperaturesByCity.csv")


dim(bfproject)


bfdata <- bfproject[8598940:8599211,]    #jan 1991 -   sep 2013                                   

bfdata <- na.omit(bfdata) #removing NA's

#Changing the date format and then dividing it into year and month respectively

bfdata$dt <-as.Date(bfdata$dt)
bfdata$Year <- format(bfdata$dt,"%Y")
bfdata$Month <- format(bfdata$dt,"%m")

head(bfdata)                         


dim(bfdata) #272 
str(bfdata)

summary(bfdata)

#Histogram/Density plot of the data
ggplot(bfdata,aes(x=AverageTemperature))+geom_histogram(bins = 50, aes(y=..density..),col = "red", fill="blue", alpha=0.3)+geom_density(col="red")

#Converting to Time Series data from data frame, inorder to proceed with time series anlysis and forecasting.
globaltemp_ts <-ts(bfdata[,2],start=c(1993,1),end=c(2013,12),frequency=12)

#Plotting the graph to check a pattern in the data
autoplot(globaltemp_ts) + ggtitle("Time Plot for Global Tempertaure") + xlab("Year") + ylab(" Average Temperature")

#In season plot we observe temperatures rising from Jan to Aug and then starts decreasing from Sept onwards
#This cycle is followed every year
ggseasonplot(globaltemp_ts)

#Time Series of the above plot seems to be have both seasonal and cyclic pattern. 
ggseasonplot(globaltemp_ts,polar=TRUE)

#We plot ggsubseriesplot to check for seasonality per month
ggsubseriesplot(globaltemp_ts) + ylab("AverageTemperature") + ggtitle("Seasonal subseries plot:Average Temperature")

#We plot the scatter plot to observe any pattern over the years
qplot(dt, AverageTemperature, data=(bfdata)) + ylab("AverageTemperature") + xlab("Years")
# As the time-frame is 20 period we do not get a very clear picture yet we can observe some sesonality over the years

#Plotting the average temperatures by month for a span of 20 years we observe June and August have highest tempertaures all over the world
ggplot(bfdata,aes(x=dt,y=AverageTemperature,colour=reorder(Month,-AverageTemperature,mean)))+
  geom_point()+
  geom_smooth(method="loess")+
  labs(title="Average Temperatures by Month",
       x="Year",
       y="Average Temperature",
       colour="Month")

#Plotting the lag plot for temp vs months
gglagplot(globaltemp_ts)
#Maximum autocorrelation exists at lag 12
ggAcf(globaltemp_ts)

#Dividing the dataset into testing and training data for model application

#Train

temp_trn <- ts(bfdata$AverageTemperature, start = c(1991,1) , end= c(2010,7), frequency = 12)

#Test
temp_tst <- ts(bfdata$AverageTemperature, start = c(2010,8) , end= c(2013,08), frequency = 12)


#TSLM - Time Series Regression 
lr_model = tslm(temp_trn ~ trend + season)
fc <- forecast(lr_model, h=36)
accuracy(fc, temp_tst)
autoplot(fc) + ylab("Average Temperature") + xlab("Year")

#Seasonal Naive Method

Naive_tmp <- snaive(temp_trn, 36)

autoplot(Naive_tmp)

summary(Naive_tmp)

accuracy(Naive_tmp , temp_tst)


#Checking the data 
test=ur.kpss(temp_trn)
summary(test)

ndiffs(temp_trn)

#Decomposition
#Applying Moving Average for m=30
bfdata$lat_ma30 = ma(bfdata$AverageTemperature, order=30)
ggplot() +
  geom_line(data = bfdata, aes(x = dt, y = AverageTemperature, colour = "Counts")) +
  geom_line(data = bfdata, aes(x = dt, y = lat_ma30, colour = "Monthly Moving Average"))  +
  ylab('Land Average Temperature')

# Moving Averages 2x12 model
plot(temp_trn, main ="2X12 MA model", ylab="Average Temperature", xlab="year")
lines(ma(ma(temp_trn,2),12),col="blue")
ma_12 = forecast(ma(ma(temp_trn,2),12), h = 36)
accuracy(ma_12, temp_tst)

#STL decomposition
delat_ma = ts(na.omit(temp_trn), frequency=30)
decomlat = stl(delat_ma, s.window="periodic")
plot(decomlat)

#Naive Forecast of seasonally adjusted data
deseasonal_cnt <- seasadj(decomlat)
decomlat %>% seasadj() %>% naive() %>%
  autoplot() + ylab("Average Temperature") + ggtitle("Naive forecasts of seasonally adjusted data")
#Forecast of time-decomposition data
fcast <- forecast(decomlat, method="naive", h=36)

plot(fcast)


#SES: Simple Exponential Smoothing
plot(temp_trn)
fit1 <-ses(temp_trn, alpha=0.2, initial="simple", h=36)
fit2 <-ses(temp_trn, alpha=0.6, initial="simple", h=36)
fit3 <-ses(temp_trn, h=36)
plot(fit1, plot.conf=FALSE, fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(lambda == 0.2), expression(lambda == 0.6),
         expression(lambda == 0.99)),pch=1)
fit3$model
x = forecast(fit3, h=36)
accuracy(x, temp_tst)

#Holt Winter's Seasonal Method
fit1 <- hw(temp_trn,seasonal="additive")

autoplot(temp_trn) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Average Temprature") +
  ggtitle("Global Average Temperature Forecast") +
  guides(colour=guide_legend(title="Forecast"))
forecast(fit1)
accuracy(fit1, temp_tst)


#ARIMA
fit <- auto.arima(temp_trn)
fit
#Forescasting the values
fitarima = forecast(fit, h = 36)
plot(fitarima)

#Accuracy of the ARIMA
accuracy(fitarima, temp_tst)
tsdisplay(residuals(fit), lag.max=10, main='Forecast Model Residuals')



