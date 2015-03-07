#The codes may take a while to complete and come up with results since it will traverse and record all the possible conbinations to make up for vwap
source("subset_sum.R")
# install.packages('chron')
# install.packages('DataCombine')
library(chron) 
library(DataCombine)

#Import the document lob and vwap
lob <- read.csv("lob.csv", header = TRUE)
vwap <- read.csv("vwap.csv", header = TRUE)

#convert date and time in lob and vwap
lob$date <- as.Date(as.character(lob$date),"%Y%m%d")
vwap$DATE <- as.Date(as.character(vwap$DATE),"%Y%m%d")

lob$time <- times(as.character(lob$time))
lob$interval <- times(as.character(lob$interval))
vwap$INTRVL_BEGTIME <- times(as.character(vwap$INTRVL_BEGTIME))

#isolate buy and sale in lob and calcualte expect price for each
lob['amount'] <- lob$size*lob$prc
lobb <- lob[row(lob)[which((lob$bs == 0)&(lob$size<=10000))],]
lobs <- lob[row(lob)[which((lob$bs == 1)&(lob$size<=10000))],]

#use random seed to choose limited number of records at certain time and certain date 
#traverse all the combination in random selected set to consist 10000 shares and calcuate the corresponding expected vwap of the combiantion
#store the expected minimum vwap and maximum vwap along with the record for corresponding combination 
buy <- data.frame(date = character(0), interval = character(0), buy_max_vwap= numeric(0), buy_min_vwap = numeric(0), buy_max_com = character(0), buy_min_com = character(0))
sell <- data.frame(date = character(0), interval = character(0), sell_max_vwap= numeric(0), sell_min_vwap = numeric(0), sell_max_com = character(0), sell_min_com = character(0))


lobb_date <- c(unique(lobb[,"date"]))
for (i in 1:length(lobb_date)){
  
  lobb_interval <- c(unique(lobb[row(lobb)[which(lobb$date==lobb_date[i])],][,c("interval")]))
  print(lobb_date[i])
  
  for(j in 1:length(lobb_interval)){
    
    max<--1
    min<- 1000
    max_opt <- ""
    min_opt <- ""
    
    print(lobb_interval[j])
    set.seed(4)
    temp_lobb <- lobb[row(lobb)[which((lobb$date==lobb_date[i])&(lobb$interval==lobb_interval[j]))],]
    temp_lobb <- temp_lobb[sample(seq(from = 1, to = length(temp_lobb$symbol), by = 3), size = 20, replace = FALSE),]
    stack <- c()
    o <- subset_sum(temp_lobb,10000,1,5,stack,0)
    
    count <- 1
    while(length(o)<3){
      print(lobb_interval[j])
      set.seed(count)
      temp_lobb <- lobb[row(lobb)[which((lobb$date==lobb_date[i])&(lobb$interval==lobb_interval[j]))],]
      temp_lobb <- temp_lobb[sample(seq(from = 1, to = length(temp_lobb$symbol), by = 3), size = 20, replace = FALSE),]
      stack <- c()
      o <- subset_sum(temp_lobb,10000,1,11,stack,0)
      count <- count+1
      print('**********************************************')
    }

    for (k in 2:length(o)){
      
#       print(sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
      
      if((sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)>max){
        max <- (sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
        max_opt <- paste(as.character(o[[k]]),  collapse='&')
      }
      
      if((sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)<min){
        min <- (sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
        min_opt <- paste(as.character(o[[k]]),  collapse='&')
      }
    }
    
    buy <- rbind(buy,data.frame(date=lobb_date[i],interval=lobb_interval[j], buy_max_vwap=max, buy_min_vwap=min, buy_max_com = max_opt, buy_min_com = min_opt))
    
  }
}


lobs_date <- c(unique(lobs[,"date"]))
for (i in 1:length(lobs_date)){
  
  lobs_interval <- c(unique(lobs[row(lobs)[which(lobs$date==lobs_date[i])],][,c("interval")]))
  print(lobs_date[i])
  
  for(j in 1:length(lobs_interval)){
    
    max<--1
    min<- 1000
    max_opt <- ""
    min_opt <- ""
    
    
    print(lobs_interval[j])
    set.seed(4)
    temp_lobs <- lobs[row(lobs)[which((lobs$date==lobs_date[i])&(lobs$interval==lobs_interval[j]))],]
    temp_lobs <- temp_lobs[sample(seq(from = 1, to = length(temp_lobs$symbol), by = 3), size = 20, replace = FALSE),]
    stack <- c()
    o <- subset_sum(temp_lobs,10000,1,5,stack,0)
    
    count <- 1
    while(length(o)<3){
      print(lobs_interval[j])
      set.seed(count)
      temp_lobs <- lobs[row(lobs)[which((lobs$date==lobs_date[i])&(lobs$interval==lobs_interval[j]))],]
      temp_lobs <- temp_lobs[sample(seq(from = 1, to = length(temp_lobs$symbol), by = 3), size = 20, replace = FALSE),]
      stack <- c()
      o <- subset_sum(temp_lobs,10000,1,11,stack,0)
      count <- count+1
      print('**********************************************')
    }  

    for (k in 2:length(o)){
      
#       print(sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
      
      
      if((sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)>max){
        max <- (sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
        max_opt <- paste(as.character(o[[k]]),  collapse='&')
      }
      
      if((sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)<min){
        min <- (sum(lob[as.numeric(c(o[[k]])),]$amount)/10000)
        min_opt <- paste(as.character(o[[k]]),  collapse='&')
      }
    }

    sell <- rbind(sell,data.frame(date=lobs_date[i],interval=lobs_interval[j], sell_max_vwap=max, sell_min_vwap=min, sell_max_com = max_opt, sell_min_com = min_opt))
  }
}
#merge sell record and buy record
output_buy_sell <- merge(buy, sell, by = c("date","interval"), all.x=TRUE)
output_buy_sell$symbol <- 'AA'

#calculate 15 min return from vwap dataset
vwap <- slide(vwap, Var = "VW_AVG_PRICE", slideBy = -1)
vwap$vwap_15min_return <- NA
for(i in 2:length(vwap$SYMBOL)){
  if((vwap$SYMBOL[i]==vwap$SYMBOL[i-1])&(vwap$DATE[i]==vwap$DATE[i-1])){
    vwap$vwap_15min_return[i] <- (vwap$VW_AVG_PRICE[i]-vwap$"VW_AVG_PRICE-1"[i])/vwap$"VW_AVG_PRICE-1"[i]
  }
}

#create key word of lag of 15 min to merge vwap return with expected vwap from lob
bs_output<-output_buy_sell
for(i in 1:length(bs_output$interval)){
  if((minutes(bs_output$interval[i])+1+15)==60){
    bs_output$minute[i] <- 0
    bs_output$hour[i] <- hours(bs_output$interval[i])+1
  }else if((minutes(bs_output$interval[i])+1+15)==75){
    bs_output$minute[i] <- 15
    bs_output$hour[i] <- hours(bs_output$interval[i])+1
  }else{
    bs_output$hour[i] <- hours(bs_output$interval[i])
    bs_output$minute[i] <- minutes(bs_output$interval[i])+1+15
  }  
}


vwap_output <- vwap
vwap_output$date <- vwap_output$DATE
vwap_output$hour <- hours(vwap_output$INTRVL_BEGTIME)
vwap_output$minute <- minutes(vwap_output$INTRVL_BEGTIME)


output <- merge(x=vwap_output, y=bs_output, by = c("date","hour","minute"), all.x=TRUE)

order_output <- output[order(output[,4], output[,5], output[,6]), ]

#save the file  into csv
fileout <- paste("ExpectedVwap_ReturnVwap_20090803_20090831.csv", sep="")
write.table(order_output[,c("SYMBOL","DATE","INTRVL_BEGTIME","vwap_15min_return","buy_max_vwap","buy_min_vwap","sell_max_vwap","sell_min_vwap")], file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

fileout <- paste("ExpectedVwap_combination_20090803_20090831.csv", sep="")
write.table(output_buy_sell, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

fileout <- paste("ReturnVwap_20090803_20090831.csv", sep="")
write.table(vwap, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")