#Auto Arima Forecasting in R
install.packages("forecast")
install.packages("data.table")
library(readxl)
library(forecast)
library(tseries)
library(stats)
library(datasets)
library(data.table)

#load the dataset from excel using readxl
hk <- read_xlsx("/Users/tamm2005/Documents/POLYU_AE/Year_3/SEM2/AMA465/Project/0005.HK (1).xlsx")

#plot the time series graph
plot.ts(hk,ylab='Price($)',xlab='Day',main='Time Series Plot of Stock Prices')

#acf and pacf of 0005.hk
hk_acf<-acf(hk,main='Autocorrelation Function for 0005.hk 
    (with 5% significance limits for the autocorrelations)',lag.max=30,ci.type='ma')
hk_acf
hk_pacf<-pacf(hk,main='Partial Autocorrelation Function for 0005.hk 
    (with 5% significance limits for the autocorrelations)',lag.max=30)
hk_pacf

#determine the stationary
adf.test(hk$Close)
diff_hk=diff((hk$Close))
adf.test(diff_hk)

library(urca)
summary(ur.kpss((hk$Close)))

summary(ur.kpss(diff(hk$Close)))
plot.ts(diff((hk$Close)))

#new acf and pacf
hk_acf<-acf(diff_hk,main='Autocorrelation Function for 0005.hk 
    (with 5% significance limits for the autocorrelations)',lag.max=30,ci.type='ma')
hk_acf<-pacf(diff_hk,main='Partial Autocorrelation Function for 0005.hk 
    (with 5% significance limits for the autocorrelations)',lag.max=30,ylim = c(0, 1))
hk_pacf

# find best ARMA(p,d,q) model
hk.arima<-auto.arima(hk)
hk.arima
hk.arima2<-arima(hk,order=c(3,1,0))
hk.arima3<-arima(hk,order=c(3,1,2))
hk.arima4<-arima(hk,order=c(2,1,2))
hk.arima5<-arima(hk,order=c(4,1,2))
list("arima(3,1,1) AIC"=hk.arima$aic,"arima(3,1,0) AIC"=hk.arima2$aic,"arima(3,1,2) AIC"=hk.arima3$aic,"arima(2,1,2) AIC"=hk.arima4$aic,"arima=(4,1,2) AIC"=hk.arima5$aic)

#arima(3,1,1) residuals
checkresiduals(hk.arima)
summary(hk.arima)
qqnorm(Box-Pierce)
qqline(hk.arima$residuals)
Box.test(hk.arima$residuals,type='Box-Pierce')

#residuals analysis
hk.arima$residuals
No <- nrow(hk)
No
SS <- sum((hk.arima$residuals)^(2))
SS
MS <- mean(hk.arima$residuals^2)
MS

#modified box-Pierce(Ljung-Box) Chi-Square statistics
Box.test(hk.arima$residuals,type="Box-Pierce",lag=12)
Box.test(hk.arima$residuals,type="Box-Pierce",lag=24)
Box.test(hk.arima$residuals,type="Box-Pierce",lag=36)
Box.test(hk.arima$residuals,type="Box-Pierce",lag=48)

#acf of ARIMA(3,1,1)
final_acf=acf(hk.arima$residuals,main='Autocorrelation Function for ARIMA(3,1,1)-Residuals  
    (with 5% significance limits for the autocorrelations)',lag.max=30,ci.type='ma')
final_acf

#Box-Pierce statistics and Ljung-Box statistics
Pierce <- ((0.008)^(2)+(-0.002)^(2)+-0.006^2+(-0.008)^2+(-0.064)^2+(-0.066)^2+0.067^2+0.012^2+(-0.023)^2+0.015^2+(-0.022)^2+0.073^2)*249
Pierce
Ljung <-((0.008)^(2)/(249-1)+(-0.002)^(2)/(249-2)+-0.006^2/(249-3)+(-0.008)^2/(249-4)+(-0.064)^2/(249-5)+(-0.066)^2/(249-6)+0.067^2/(249-7)+0.012^2/(249-8)+(-0.023)^2/(249-9)+0.015^2/(249-10)+(-0.022)^2/(249-11)+0.073^2/(249-12))*249*(249+2)
Ljung
qchisq(0.95,df=9)



