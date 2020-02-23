# install.packages("readxl")
library(readxl)
library(forecast)
tsData  = ts(data = sars_dataset$`%chinese`, start = c(2001,1),frequency = 12)
plot(tsData,xlab = "Year.Month" ,ylab = "Ratio of Chinese travellers",sub="Initial Chinese visitos around SARS outbreak")
grid(nx=50,ny=50, col = "lightgrey")
autoarima1<-auto.arima(tsData)
autoarima1

acf(tsData)
pacf(tsData)
arima_model<-arima(tsData,order=c(0,1,0),seasonal = c(0,0,1))
prediction<-forecast(arima_model,level = c(95),h=12)
plot(prediction,xlab = "Year.Month" ,ylab = "Ratio of Chinese travellers",sub="Prediction of Chinese visitos after outbreak")
grid(nx=50,ny=50, col = "lightgrey")


cycle(tsData)
boxplot(tsData~cycle(tsData),sub="box plot to observe the medians for seasonality")

ddata<-decompose(tsData)
plot(ddata)
grid(nx=50,ny=50, col = "lightgrey")

plot(tsData,xlab = "Year.Month" ,ylab = "Ratio of Chinese travellers",sub="Fitting curve to the dataset")
grid(nx=50,ny=50, col = "lightgrey")
abline(reg=lm(tsData~time(tsData)))
cycle(tsData)

Box.test(arima_model$resid,lag=5,type="Ljung-Box")
tsCorona <- ts(data = corona_dataset$`%chinese`,start = c(2018,1),frequency = 12)
refit<-Arima(tsCorona,model = arima_model)
forcaster<-forecast(refit,level=c(95),h=12)
plot(forcaster,xlab = "Year.Month" ,ylab = "Ratio of Chinese travellers")
grid(nx=50,ny=50, col = "lightgrey")
