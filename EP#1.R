#Empirical Project Part 1

#Clean your workspace
rm(list=ls())
dev.off()
#Press Ctrl+L

#Checking Working Directory
getwd()
setwd("C:/Users/Пользователь/Desktop/ECON 413/Codes")

#Upload the data
dataset <- read.table("dataset_EP.txt", header = T)


#introduce new variables
dataset$dIR = dataset$IR - c(NA,dataset$IR[-length(dataset$IR)])
dataset$lIP = log(dataset$IP)
dataset$gIP = dataset$lIP - c(NA,dataset$lIP[-length(dataset$lIP)])

summary (dataset)

#Order data

dataset$Date
dataset$Date <- as.Date(dataset$Date)
dataset<- dataset[order(dataset$Date),]

#BUild time series scatter plot for log(IP) variable
library(ggplot2)

ggplot(dataset, aes(x=Date, y=lIP))+ 
  geom_line(lwd=1.0)+
 #geom_hline(yintercept = 0, lwd = 0, color="black" )+
  xlab("Time")+
  ylab("Logarithm of Industry Production Index")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
)


#BUild time series scatter plot for First Difference of log(IP) variable

ggplot(dataset, aes(x=Date, y=gIP))+
  geom_line(lwd=1.0)+
  geom_hline(yintercept = 0, lwd = 0, color="black" )+
  xlab("Time")+
  ylab("Growth Rate of Industry Production Index")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
  )

#BUild time series scatter plot for IR variable

ggplot(dataset, aes(x=Date, y=IR))+
  geom_line(lwd=1.0)+
  geom_hline(yintercept = 0, lwd = 0, color="black" )+
  xlab("Time")+
  ylab("Interest Rate")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
  )

#BUild time series scatter plot for the first difference of IR variable

ggplot(dataset, aes(x=Date, y=dIR))+
  geom_line(lwd=1.0)+
  geom_hline(yintercept = 0, lwd = 0, color="black" )+
  xlab("Time")+
  ylab("first difference of Interest Rate")+
  scale_x_date(date_labels = "%Y-%m")+
  theme_classic()+
  theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 10, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.title = element_text(size = 10, face = "bold")
  )


