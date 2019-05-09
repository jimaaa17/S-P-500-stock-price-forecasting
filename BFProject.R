library(fpp)
library(fpp2)
library(ggplot2)
library(forecast)
library(urca)
#library(caTools)
View(GSPC)
hist(GSPC$Close)
hist(GSPC$Close, 
     main="Histogram for GSPC$Close", 
     xlab="GSPC$Close", 
     border="blue", 
     col="green")
x <- GSPC$Close
y <- GSPC$Volume
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)

#Convert it into time series
GSPC1<- ts(GSPC$Adj.Close, start=c(2000, 1),end=c(2018,12), freq=12)
GSPC1

#Plot the time series with ACF and PACF
plot(GSPC1)
ggAcf(GSPC1)
ggPacf(GSPC1)

#To check if it stationary or not
test_stationary=(ur.kpss(GSPC1))
summary(test_stationary)
#we obtain that the test statistic i.e.3.1593 is much bigger than the 1% critical value=0.739, indicating that the null hypothesis is
#rejected.That is, the data are not stationary. 
ndiffs(GSPC1)

#Seasonal Plot
ggseasonplot(GSPC1, year.labels=TRUE, year.labels.left=TRUE) + ylab("Adj.Close") + ggtitle("GSPC1")

#Different forecastingb methods with graphs
meanf(GSPC1,h=24)
naive(GSPC1,h=24)
snaive(GSPC,h=24)
rwf(GSPC1,h=24,drift=TRUE)
autoplot(GSPC1) +
  autolayer(meanf(GSPC1, h=24),
            series="Mean", PI=FALSE) +
  ggtitle("GSPC(monthly stock ending Dec 2018)") +
  xlab("Year") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(GSPC1) +
  autolayer(naive(GSPC1, h=24),
            series="Naïve", PI=FALSE) +
  ggtitle("GSPC(monthly stock ending Dec 2018)") +
  xlab("Year") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(GSPC1) +
  autolayer(snaive(GSPC1, h=24),
            series="SNaïve", PI=FALSE) +
  ggtitle("GSPC(monthly stock ending Dec 2018)") +
  xlab("Year") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))
autoplot(GSPC1) +
  autolayer(rwf(GSPC1,drift=TRUE, h=24),
            series="Drift", PI=FALSE) +
  ggtitle("GSPC(monthly stock ending Dec 2018)") +
  xlab("Year") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

#Accuracy of forecasting methods
GSPC_train<- window(GSPC1,start=2000,end=c(2016,12))
GSfit1 <- meanf(GSPC_train,h=24)
GSfit2 <- rwf(GSPC_train,h=24,drift=TRUE)
GSfit2
GSfit3 <- snaive(GSPC_train,h=24)
GSfit3
autoplot(GSPC_train) +
  autolayer(GSfit1, series="Mean", PI=FALSE) +
  autolayer(GSfit2, series="Drift", PI=FALSE) +
  autolayer(GSfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Adj.Close") +
  ggtitle("Forecasts for monthly GSPC") +
  guides(colour=guide_legend(title="Forecast"))
GSPC_test <- window(GSPC1, start=2016,end=2018)
accuracy(GSfit1, GSPC_test)
accuracy(GSfit2, GSPC_test)
accuracy(GSfit3, GSPC_test)

#To find smoothing parameters for SES
fit1=ses(GSPC1,alpha=0.9,h=24)
plot(fit1)
fit1$model
fit6=ses(GSPC_train,alpha=0.9,h=24)
fit6$model
accuracy(fit6,GSPC_test)

#Holt's Method
GSPC2 <- window(GSPC1, start=2000) 
fc <- holt(GSPC2, h=24)
fc
autoplot(GSPC2) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Adj.Close") +
  guides(colour=guide_legend(title="Forecast"))
fc1=holt(GSPC_train,h=24)
accuracy(fc1,GSPC_test)

#To find the smoothing parameters for Holt fuction
fc$model


#Arima Models
Arima(GSPC1,order=c(1,0,0))
Arima(GSPC1,order=c(0,0,1))
Arima(GSPC1,order=c(0,1,0))
fit2<- auto.arima(GSPC1,seasonal=FALSE)
fit2

#Forecasting the ARIMA Model
fit3=forecast(fit2,h=24)
plot(fit3)

fit4=auto.arima(GSPC_train,seasonal=FALSE)
fit4
fit5=forecast(fit4,h=24)
accuracy(fit5,GSPC_test)

