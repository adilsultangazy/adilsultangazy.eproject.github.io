#Empirical Project Part 2

#Clean your workspace
rm(list=ls())
dev.off()
#Press Ctrl+L

#Checking Working Directory
getwd()
setwd("C:/Users/Пользователь/Desktop/ECON 413/Codes")

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
dataset$gIP = dataset$lIP - c(NA,dataset$lIP[-length(dataset$lIP)])

# introduce lag variables for gIP
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

summary (dataset)
class(dataset)
str(dataset)

#install.packages("zoo")
library(zoo)
library(astsa)
library(ggplot2)
library(xts)

#Part 1: Draw AutoCorrelation Functions for gIP and dIR

#Plot Autocorrelation for gIP
data_ts_gIP <- xts(dataset$gIP,dataset$Date)
class(data_ts_gIP)
str(data_ts_gIP)
acf(data_ts_gIP, lag.max = 10, na.action = na.pass)
acf(data_ts_gIP, lag.max = 100, na.action = na.pass)

#Plot Autocorrelation for dIR
data_ts_dIR <- xts(dataset$dIR,dataset$Date)
class(data_ts_dIR)
str(data_ts_dIR)
acf(data_ts_dIR, lag.max = 10, na.action = na.pass)
acf(data_ts_dIR, lag.max = 100, na.action = na.pass)

#AR(1) model for gIP
AR1_gIP <- arima(dataset$gIP, order=c(1,0,0))
print(AR1_gIP)
AR1_fit_gIP <- dataset$gIP - residuals(AR1_gIP)

#Plot AR(1) model for gIP
ggplot()+ 
  geom_line(data = dataset,aes(x=Date, y=gIP), color = "black",lwd=0.8)+
  geom_line(data = dataset,aes(x=Date, y=AR1_fit_gIP), color = "red",lwd=0.8)+
  geom_hline(yintercept = 0, lwd = 0, color="blue" )+
  xlab("Time")+
  ylab("AR(1) of Growth Rate of Industry Production Index")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
  )


#AR(1) model for dIR
AR1_dIR <- arima(dataset$dIR, order=c(1,0,0))
print(AR1_dIR)
AR1_fit_dIR <- dataset$dIR - residuals(AR1_dIR)

ggplot()+ 
  geom_line(data = dataset,aes(x=Date, y=dIR), color = "black",lwd=0.8)+
  geom_line(data = dataset,aes(x=Date, y=AR1_fit_dIR), color = "red",lwd=0.8)+
  geom_hline(yintercept = 0, lwd = 0, color="blue" )+
  xlab("Time")+
  ylab("AR(1) of the first difference of Interest Rate")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
  )


#Part 2: estimate the AR(1),...,AR(10) models for both gIP,dIR series

#ARIMA:AR(1),...,AR(10) models for gIP

#AR1
AR1_gIP <- arima(dataset$gIP, order=c(1,0,0))
print(AR1_gIP)
AR1_fit_gIP <- dataset$gIP - residuals(AR1_gIP)
ts.plot(dataset$gIP)+
points(AR1_fit_gIP, type="l", col=2, lty = 2)

#########################
AR1_gIP 

dataset$gIP_1 = c(NA,dataset$gIP[-length(dataset$gIP)])
AR1_gIP_test = lm(gIP ~ gIP_1, data=dataset)
summary(AR1_gIP_test)
dataset$uhat_gIP_1 = AR1_gIP_test$residuals

dataset$uhat_gIP_1
residuals(AR1_gIP)
###############################

#AR2
AR2_gIP <- arima(dataset$gIP, order=c(2,0,0))
print(AR2_gIP)
AR2_fit_gIP <- dataset$gIP - residuals(AR2_gIP)
ts.plot(dataset$gIP)+
  points(AR2_fit_gIP, type="l", col=2, lty = 2)

#AR3
AR3_gIP <- arima(dataset$gIP, order=c(3,0,0))
print(AR3_gIP)
AR3_fit_gIP <- dataset$gIP - residuals(AR3_gIP)
ts.plot(dataset$gIP)+
  points(AR3_fit_gIP, type="l", col=2, lty = 2)

#AR4
AR4_gIP <- arima(dataset$gIP, order=c(4,0,0))
print(AR4_gIP)
AR4_fit_gIP <- dataset$gIP - residuals(AR4_gIP)
ts.plot(dataset$gIP)+
  points(AR4_fit_gIP, type="l", col=2, lty = 2)

#AR5
AR5_gIP <- arima(dataset$gIP, order=c(5,0,0))
print(AR5_gIP)
AR5_fit_gIP <- dataset$gIP - residuals(AR5_gIP)
ts.plot(dataset$gIP)+
  points(AR5_fit_gIP, type="l", col=2, lty = 2)

#AR6
AR6_gIP <- arima(dataset$gIP, order=c(6,0,0))
print(AR2_gIP)
AR6_fit_gIP <- dataset$gIP - residuals(AR6_gIP)
ts.plot(dataset$gIP)+
  points(AR6_fit_gIP, type="l", col=2, lty = 2)

#AR7
AR7_gIP <- arima(dataset$gIP, order=c(7,0,0))
print(AR7_gIP)
AR7_fit_gIP <- dataset$gIP - residuals(AR7_gIP)
ts.plot(dataset$gIP)+
  points(AR7_fit_gIP, type="l", col=2, lty = 2)

#AR8
AR8_gIP <- arima(dataset$gIP, order=c(8,0,0))
print(AR8_gIP)
AR8_fit_gIP <- dataset$gIP - residuals(AR8_gIP)
ts.plot(dataset$gIP)+
  points(AR8_fit_gIP, type="l", col=2, lty = 2)

#AR9
AR9_gIP <- arima(dataset$gIP, order=c(9,0,0))
print(AR9_gIP)
AR9_fit_gIP <- dataset$gIP - residuals(AR9_gIP)
ts.plot(dataset$gIP)+
  points(AR9_fit_gIP, type="l", col=2, lty = 2)

#AR10
AR10_gIP <- arima(dataset$gIP, order=c(10,0,0))
print(AR10_gIP)
AR10_fit_gIP <- dataset$gIP - residuals(AR10_gIP)
ts.plot(dataset$gIP)+
  points(AR10_fit_gIP, type="l", col=2, lty = 2)


#ARIMA: AR(1),...,AR(10) models for dIR

#AR1
AR1_dIR <- arima(dataset$dIR, order=c(1,0,0))
print(AR1_dIR)
AR1_fit_dIR <- dataset$dIR - residuals(AR1_dIR)
ts.plot(dataset$dIR)+
  points(AR1_fit_dIR, type="l", col=2, lty = 2)

#AR2
AR2_dIR <- arima(dataset$dIR, order=c(2,0,0))
print(AR2_dIR)
AR2_fit_dIR <- dataset$dIR - residuals(AR2_dIR)
ts.plot(dataset$dIR)+
  points(AR2_fit_dIR, type="l", col=2, lty = 2)

#AR3
AR3_dIR <- arima(dataset$dIR, order=c(3,0,0))
print(AR3_dIR)
AR3_fit_dIR <- dataset$dIR - residuals(AR3_dIR)
ts.plot(dataset$dIR)+
  points(AR3_fit_dIR, type="l", col=2, lty = 2)

#AR4
AR4_dIR <- arima(dataset$dIR, order=c(4,0,0))
print(AR4_dIR)
AR4_fit_dIR <- dataset$dIR - residuals(AR4_dIR)
ts.plot(dataset$dIR)+
  points(AR4_fit_dIR, type="l", col=2, lty = 2)

#AR5
AR5_dIR <- arima(dataset$dIR, order=c(5,0,0))
print(AR5_dIR)
AR5_fit_dIR <- dataset$dIR - residuals(AR5_dIR)
ts.plot(dataset$dIR)+
  points(AR5_fit_dIR, type="l", col=2, lty = 2)

#AR6
AR6_dIR <- arima(dataset$dIR, order=c(6,0,0))
print(AR2_dIR)
AR6_fit_dIR <- dataset$dIR - residuals(AR6_dIR)
ts.plot(dataset$dIR)+
  points(AR6_fit_dIR, type="l", col=2, lty = 2)

#AR7
AR7_dIR <- arima(dataset$dIR, order=c(7,0,0))
print(AR7_dIR)
AR7_fit_dIR <- dataset$dIR - residuals(AR7_dIR)
ts.plot(dataset$dIR)+
  points(AR7_fit_dIR, type="l", col=2, lty = 2)

#AR8
AR8_dIR <- arima(dataset$dIR, order=c(8,0,0))
print(AR8_dIR)
AR8_fit_dIR <- dataset$dIR - residuals(AR8_dIR)
ts.plot(dataset$dIR)+
  points(AR8_fit_dIR, type="l", col=2, lty = 2)

#AR9
AR9_dIR <- arima(dataset$dIR, order=c(9,0,0))
print(AR9_dIR)
AR9_fit_dIR <- dataset$dIR - residuals(AR9_dIR)
ts.plot(dataset$dIR)+
  points(AR9_fit_dIR, type="l", col=2, lty = 2)

#AR10
AR10_dIR <- arima(dataset$dIR, order=c(10,0,0))
print(AR10_dIR)
AR10_fit_dIR <- dataset$dIR - residuals(AR10_dIR)
ts.plot(dataset$dIR)+
  points(AR10_fit_dIR, type="l", col=2, lty = 2)


#Estimate BIC for gIP AR functions using ARIMA method

bic_gIP_ARIMA = numeric(10)
bic_gIP_ARIMA[1]=BIC(AR1_gIP)
bic_gIP_ARIMA[2]=BIC(AR2_gIP)
bic_gIP_ARIMA[3]=BIC(AR3_gIP)
bic_gIP_ARIMA[4]=BIC(AR4_gIP)
bic_gIP_ARIMA[5]=BIC(AR5_gIP)
bic_gIP_ARIMA[6]=BIC(AR6_gIP)
bic_gIP_ARIMA[7]=BIC(AR7_gIP)
bic_gIP_ARIMA[8]=BIC(AR8_gIP)
bic_gIP_ARIMA[9]=BIC(AR9_gIP)
bic_gIP_ARIMA[10]=BIC(AR10_gIP)
plot(bic_gIP_ARIMA, ylab = "BIC values", xlab = "Autoregression Index")
  
  
#Estimate BIC for dIR AR functions using ARIMA method

bic_dIR_ARIMA = numeric(10)
bic_dIR_ARIMA[1]=BIC(AR1_dIR)
bic_dIR_ARIMA[2]=BIC(AR2_dIR)
bic_dIR_ARIMA[3]=BIC(AR3_dIR)
bic_dIR_ARIMA[4]=BIC(AR4_dIR)
bic_dIR_ARIMA[5]=BIC(AR5_dIR)
bic_dIR_ARIMA[6]=BIC(AR6_dIR)
bic_dIR_ARIMA[7]=BIC(AR7_dIR)
bic_dIR_ARIMA[8]=BIC(AR8_dIR)
bic_dIR_ARIMA[9]=BIC(AR9_dIR)
bic_dIR_ARIMA[10]=BIC(AR10_dIR)
plot(bic_dIR_ARIMA, ylab = "BIC values", xlab = "Autoregression Index")

#Estimate BIC for dIR AR functions using linear regression model method

head(dataset, 12)

bic_gIP = numeric(10)
bic_gIP[1] = BIC(lm(gIP~gIP_1, data=dataset))
bic_gIP[2] = BIC(lm(gIP~gIP_1+gIP_2, data=dataset))
bic_gIP[3] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3, data=dataset))
bic_gIP[4] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4, data=dataset))
bic_gIP[5] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5, data=dataset))
bic_gIP[6] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6, data=dataset))
bic_gIP[7] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+gIP_7, data=dataset))
bic_gIP[8] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+gIP_7+gIP_8, data=dataset))
bic_gIP[9] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+gIP_7+gIP_8+gIP_9, data=dataset))
bic_gIP[10] = BIC(lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4+gIP_5+gIP_6+gIP_7+gIP_8+gIP_9+gIP_10, data=dataset))
plot(bic_gIP, ylab = "BIC values", xlab = "Autoregression Index")

#Estimate BIC for gIP AR functions using linear regression model method

bic_dIR = numeric(10)
bic_dIR[1] = BIC(lm(dIR~dIR_1, data=dataset))
bic_dIR[2] = BIC(lm(dIR~dIR_1+dIR_2, data=dataset))
bic_dIR[3] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3, data=dataset))
bic_dIR[4] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4, data=dataset))
bic_dIR[5] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5, data=dataset))
bic_dIR[6] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6, data=dataset))
bic_dIR[7] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7, data=dataset))
bic_dIR[8] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8, data=dataset))
bic_dIR[9] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9, data=dataset))
bic_dIR[10] = BIC(lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9+dIR_10, data=dataset))
plot(bic_dIR, ylab = "BIC values", xlab = "Autoregression Index")

#since the BIC results in both methods differ I will take the results from lm method that we used in the Practical session#3 

#for gIP take AR(1) and AR(4)
#for dIR take AR(1) and AR(9)

#Part 3: test if the disturbances from these models are serially correlated using AR(2) serial correlation test.

#testing serial correlation for gIP AR(4) using AR(2) serial correlation test.
head(dataset)
dataset1 = dataset[-(1:5),]
head(dataset1)
gIP_lm1 = lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4, data=dataset1)
dataset1$gIP_u_hat = gIP_lm1$residuals
dataset1$gIP_u_hat_1 = c(NA,dataset1$gIP_u_hat[-length(dataset1$gIP_u_hat)])
dataset1$gIP_u_hat_2 = c(NA,dataset1$gIP_u_hat_1[-length(dataset1$gIP_u_hat)])
head(dataset1)
dataset2 = dataset1[-(1:2),]
head(dataset2)
gIP_lm2 = lm(gIP_u_hat~gIP_1+gIP_2+gIP_3+gIP_4+gIP_u_hat_1+gIP_u_hat_2,data=dataset2)
gIP_lm1 = lm(gIP_u_hat~gIP_1+gIP_2+gIP_3+gIP_4,data=dataset2)
anova(gIP_lm1,gIP_lm2)  


#testing serial correlation for gIP AR(1) using AR(2) serial correlation test.
head(dataset)
dataset1 = dataset[-(1:2),]
head(dataset1)
gIP_lm1 = lm(gIP~gIP_1, data=dataset1)
dataset1$gIP_u_hat = gIP_lm1$residuals
dataset1$gIP_u_hat_1 = c(NA,dataset1$gIP_u_hat[-length(dataset1$gIP_u_hat)])
dataset1$gIP_u_hat_2 = c(NA,dataset1$gIP_u_hat_1[-length(dataset1$gIP_u_hat)])
head(dataset1)
dataset2 = dataset1[-(1:2),]
head(dataset2)
gIP_lm2 = lm(gIP_u_hat~gIP_1+gIP_u_hat_1+gIP_u_hat_2,data=dataset2)
gIP_lm1 = lm(gIP_u_hat~gIP_1,data=dataset2)
anova(gIP_lm1,gIP_lm2)  
  
#testing serial correlation for dIR AR(9) using AR(2) serial correlation test.
head(dataset)
dataset1 = dataset[-(1:10),]
head(dataset1)
dIR_lm1 = lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9, data=dataset1)
dataset1$dIR_u_hat = dIR_lm1$residuals
dataset1$dIR_u_hat_1 = c(NA,dataset1$dIR_u_hat[-length(dataset1$dIR_u_hat)])
dataset1$dIR_u_hat_2 = c(NA,dataset1$dIR_u_hat_1[-length(dataset1$dIR_u_hat)])
head(dataset1)
dataset2 = dataset1[-(1:2),]
head(dataset2)
dIR_lm2 = lm(dIR_u_hat~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9+dIR_u_hat_1+dIR_u_hat_2,data=dataset2)
dIR_lm1 = lm(dIR_u_hat~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9,data=dataset2)
anova(dIR_lm1,dIR_lm2)  

#testing serial correlation for dIR AR(1) using AR(2) serial correlation test.
head(dataset)
dataset1 = dataset[-(1:2),]
head(dataset1)
dIR_lm1 = lm(dIR~dIR_1, data=dataset1)
dataset1$dIR_u_hat = dIR_lm1$residuals
dataset1$dIR_u_hat_1 = c(NA,dataset1$dIR_u_hat[-length(dataset1$dIR_u_hat)])
dataset1$dIR_u_hat_2 = c(NA,dataset1$dIR_u_hat_1[-length(dataset1$dIR_u_hat)])
head(dataset1)
dataset2 = dataset1[-(1:2),]
head(dataset2)
dIR_lm2 = lm(dIR_u_hat~dIR_1+dIR_u_hat_1+dIR_u_hat_2,data=dataset2)
dIR_lm1 = lm(dIR_u_hat~dIR_1,data=dataset2)
anova(dIR_lm1,dIR_lm2)  


#Part 4: T=188, n=141, m=47. Reestimate models using n observations.
#Generate one-step ahead predictions using m observations. Decide which model is better.

#split data
dataset_est = dataset[1:141,]
dataset_pred = dataset[142:188,]

#forecasting gIP with AR(4) 
gIP_lm_AR4 = lm(gIP~gIP_1+gIP_2+gIP_3+gIP_4, data=dataset_est)
gIP_pred = predict(gIP_lm_AR4, newdata = dataset_pred)
pred_error = dataset_pred$gIP - gIP_pred
gIP_rmse_AR4 = sqrt(mean(pred_error^2))
gIP_mae_AR4 = mean(abs(pred_error))

#forecasting gIP with AR(1)
gIP_lm_AR1 = lm(gIP~gIP_1, data=dataset_est)
  gIP_pred = predict(gIP_lm_AR1, newdata = dataset_pred)
pred_error = dataset_pred$gIP - gIP_pred
gIP_rmse_AR1 = sqrt(mean(pred_error^2))
gIP_mae_AR1 = mean(abs(pred_error))

gIP_rmse_AR4
gIP_rmse_AR1
gIP_mae_AR4
gIP_mae_AR1

#compare two forecasting models for gIP

100*(gIP_rmse_AR1/gIP_rmse_AR4-1)
100*(gIP_mae_AR1/gIP_mae_AR4-1)

#AR(4) process is more precise to forecast gIP

#forecasting dIR with AR(9) 
dIR_lm_AR9 = lm(dIR~dIR_1+dIR_2+dIR_3+dIR_4+dIR_5+dIR_6+dIR_7+dIR_8+dIR_9, data=dataset_est)
dIR_pred = predict(dIR_lm_AR9, newdata = dataset_pred)
pred_error = dataset_pred$dIR - dIR_pred
dIR_rmse_AR9 = sqrt(mean(pred_error^2))
dIR_mae_AR9 = mean(abs(pred_error))

#forecasting gIP with AR(1)
dIR_lm_AR1 = lm(dIR~dIR_1, data=dataset_est)
dIR_pred = predict(dIR_lm_AR1, newdata = dataset_pred)
pred_error = dataset_pred$dIR - dIR_pred
dIR_rmse_AR1 = sqrt(mean(pred_error^2))
dIR_mae_AR1 = mean(abs(pred_error))

dIR_rmse_AR9
dIR_rmse_AR1
dIR_mae_AR9
dIR_mae_AR1

#compare two forecasting models for dIR
100*(dIR_rmse_AR9/dIR_rmse_AR1-1)
100*(dIR_mae_AR9/dIR_mae_AR1-1)

#AR(1) process is more precise to forecast dIR



