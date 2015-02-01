source("C:/Users/Ethan/Documents/mydata/codes/codes2.0/importData.R")
source("C:/Users/Ethan/Documents/mydata/codes/codes2.0/checkData.R")
source("C:/Users/Ethan/Documents/mydata/codes/codes2.0/processData.R")
source("C:/Users/Ethan/Documents/mydata/codes/codes2.0/plotData.R")
source("C:/Users/Ethan/Documents/mydata/codes/codes2.0/analysisData.R")

time1 <- "2014-12-01"#start time
time2 <- "2014-12-31"#end time
data<-analysisData(time1,time2)
smadata <- data$sma#data for sma from database1.3
spydata <- data$spy#data for spy from yahoo finance
output <- data$output#data for processed sma and spy

#plotData(output) plot function

library(reshape)
library(ggplot2)
pdf(file= paste('C:/Users/Ethan/Documents/mydata/results/plot2.0from',time1,'to',time2,'.pdf'), width=10, height=6.18)
par(mfrow=(c(1,6)))


mydata1 <- output[,c(1,3,5,6)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("red","green", "black")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("Average Return") +
  ggtitle(paste("AVERAGE DAILY RETURN\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,7,9,11)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("red","green", "black")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("Cumulative Return") +
  ggtitle(paste("CUMULATIVE RETURN\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,8,10,12)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("red","green", "black")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("Cumulative S-Score") +
  ggtitle(paste("CUMULATIVE S-Score\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,13,14,15)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("green","red", "blue")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("Return Spread") +
  ggtitle(paste("RETURNS SPREAD\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,15)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("blue")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("Return Spread") +
  ggtitle(paste("RETURN SPREAD\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,16)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("blue")) +
  theme(legend.position="bottom") +
  xlab("Date") + ylab("S-score Gap Ratio") +
  ggtitle(paste("S-Score GAP RATIO\r\n",time1," TO ",time2))

mydata1 <- output[,c(1,17,18,19)]
mydata1 <- melt(mydata1, id=c("center_date"))
ggplot(data=mydata1, aes(x=center_date, y=value, group=variable, colour=variable)) + 
  geom_line() + 
  scale_colour_manual(values = c("green","red", "black")) +
  theme(legend.position="bottom") +
  xlab("Time") + ylab("S-score change ratio") +
  ggtitle(paste("S-Score CHANGE RATIO FROM\r\n",time1," TO ",time2))

dev.off()


