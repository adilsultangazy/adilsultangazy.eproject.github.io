#Empirical Project Part 3 

#Clean your workspace
rm(list=ls())
dev.off()
#Press Ctrl+L

#Checking Working Directory
getwd()

#Upload the data
dataset <- read.table("dataset_EP.txt", header = T)

#Order data
dataset$Date
dataset$Date <- as.Date(dataset$Date, "%Y-%m-%d")
#dataset<- dataset[order(dataset$Date),]

class(dataset)
str(dataset)

#convert dataframe to time series
#data_ts <- xts(dataset$IP,dataset$Date)
#is.ts(data_ts)

#class(data_ts)
#str(dataset)

#introduce new variables
dataset$dIR = dataset$IR - c(NA,dataset$IR[-length(dataset$IR)])
dataset$lIP = log(dataset$IP)
dataset$gIP = 100*(dataset$lIP - c(NA,dataset$lIP[-length(dataset$lIP)]))

#introducing gIP lag variables
dataset$gIP_1 = c(NA,dataset$gIP[-length(dataset$gIP)])
dataset$gIP_2 = c(NA,dataset$gIP_1[-length(dataset$gIP)])
dataset$gIP_3 = c(NA,dataset$gIP_2[-length(dataset$gIP)])
dataset$gIP_4 = c(NA,dataset$gIP_3[-length(dataset$gIP)])
dataset$gIP_5 = c(NA,dataset$gIP_4[-length(dataset$gIP)])
dataset$gIP_6 = c(NA,dataset$gIP_5[-length(dataset$gIP)])
dataset$gIP_7 = c(NA,dataset$gIP_6[-length(dataset$gIP)])
dataset$gIP_8 = c(NA,dataset$gIP_7[-length(dataset$gIP)])
dataset$gIP_9 = c(NA,dataset$gIP_8[-length(dataset$gIP)])
dataset$gIP_10 = c(NA,dataset$gIP_9[-length(dataset$gIP)])

# introduce lag variables for dIR 
dataset$dIR_1 = c(NA,dataset$dIR[-length(dataset$dIR)])
dataset$dIR_2 = c(NA,dataset$dIR_1[-length(dataset$dIR)])
dataset$dIR_3 = c(NA,dataset$dIR_2[-length(dataset$dIR)])
dataset$dIR_4 = c(NA,dataset$dIR_3[-length(dataset$dIR)])
dataset$dIR_5 = c(NA,dataset$dIR_4[-length(dataset$dIR)])
dataset$dIR_6 = c(NA,dataset$dIR_5[-length(dataset$dIR)])
dataset$dIR_7 = c(NA,dataset$dIR_6[-length(dataset$dIR)])
dataset$dIR_8 = c(NA,dataset$dIR_7[-length(dataset$dIR)])
dataset$dIR_9 = c(NA,dataset$dIR_8[-length(dataset$dIR)])
dataset$dIR_10 = c(NA,dataset$dIR_9[-length(dataset$dIR)])

#part1: find IP and LRP for general ARDL model
lm_IP = lm(gIP~dIR, data=dataset)
summary(lm_IP) 
lm_LRP = lm(gIP~gIP_1+gIP_2+dIR+dIR_1+dIR_2, data=dataset)
summary(lm_LRP)
coef=lm_LRP$coefficients
LRP = (sum(coef[4:6]))/(1-sum(coef[2:3]))

#part2&3: estimate ARDL(6,6) by OLS and calculate IP and LRP

lm_ols = lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+dIR+dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6, data=dataset)
summary(lm_ols)
coefs = lm_ols$coefficients
coefs
IP = coefs[[8]]
LRP = (sum(coefs[8:14]))/(1-sum(coefs[2:7]))

#part#4: testing for AR(2) autocorrelation from ARDL(6,6)
dataset1 = dataset[-(1:7),]
head(dataset1)
ARDL_lm1 = lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+dIR+dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6, data=dataset1)
dataset1$u_hat = ARDL_lm1$residuals
dataset1$u_hat_1 = c(NA,dataset1$u_hat[-length(dataset1$u_hat)])
dataset1$u_hat_2 = c(NA,dataset1$u_hat_1[-length(dataset1$u_hat)])
head (dataset1)
dataset2 = dataset1[-(1:2),]
head(dataset2)
lm1 = lm(u_hat~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+dIR+dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6, data=dataset2)
lm2 = lm(u_hat~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+dIR+dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+u_hat_1+u_hat_2, data=dataset2)
anova(lm1,lm2)

#part#5: Plot ACF for log of Industrial production and 1st diff. of interest rate
#and compare results for AR model 

library(xts)
#Plot Autocorrelation for log(IP)
data_ts_lIP <- xts(dataset$lIP,dataset$Date)
acf(data_ts_lIP, lag.max = 10, na.action = na.pass)
acf(data_ts_lIP, lag.max = 400, na.action = na.pass)

#Plot Autocorrelation for first difference of log(IP)
data_ts_gIP <- xts(dataset$gIP,dataset$Date)
acf(data_ts_gIP, lag.max = 10, na.action = na.pass)
acf(data_ts_gIP, lag.max = 100, na.action = na.pass)

#Plot Autocorrelation for IR
data_ts_IR <- xts(dataset$IR,dataset$Date)
acf(data_ts_IR, lag.max = 10, na.action = na.pass)
acf(data_ts_IR, lag.max = 100, na.action = na.pass)

#Plot Autocorrelation for the first difference of IR
data_ts_dIR <- xts(dataset$dIR,dataset$Date)
acf(data_ts_dIR, lag.max = 10, na.action = na.pass)
acf(data_ts_dIR, lag.max = 100, na.action = na.pass)

#part6: testing for unit roots
#install.packages("vars")
library(vars)
ur_test_IP = ur.df(dataset$IP, type = "trend", lags = 4)
summary(ur_test_IP)
ur_test_IR = ur.df(dataset$IR, type = "trend", lags = 4)
summary(ur_test_IR)
dataset1 = dataset[-1,]
ur_test_gIP = ur.df(dataset1$gIP, type = "trend", lags = 4)
summary(ur_test_gIP)
ur_test_dIR = ur.df(dataset1$dIR, type = "trend", lags = 4)
summary(ur_test_dIR)

#part7: forecasting with ARDL(6,6)
dataset_est = dataset_est = dataset[1:141,]
dataset_pred = dataset[142:188,]
lmf_ARDL = lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6, data=dataset_est)
gIP_pred = predict(lmf_ARDL, newdata = dataset_pred)
pred_error = dataset_pred$gIP - gIP_pred
rmse1 = sqrt(mean(pred_error^2))
mae1 = mean(abs(pred_error))
rmse1/100
mae1/100
(0.03947916-0.03326386)/0.03326386
(0.03263953-0.02755496)/0.02755496
