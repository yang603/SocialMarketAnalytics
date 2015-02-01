importData <- function(date1, date2){
  #######################################################
  #import the sma data from database system
  
  library("RODBC")
  
  options(digits=4)
  
  odbcCloseAll()
  ch = odbcConnect("Falcon_3326")
  
  query <- paste("
                 SET time_zone='US/Eastern';
                 ")
  sqlQuery(ch, query)
  
  query <- paste("
                 SET Transaction Isolation Level Read Uncommitted;
                 ")
  sqlQuery(ch, query)
  
  
  query_sma <- paste("
                     SELECT qbt.center_date
                     , sum(case when sbt.ew_sent_z < -2 then 1 else 0 end) as sent_tickers_m2
                     , sum(case when sbt.ew_sent_z < -2 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.ew_sent_z < -2 then 1 else 0 end) as m2_avg_return
                     , sum(case when sbt.ew_sent_z < -1 and sbt.ew_sent_z >= -2 then 1 else 0 end) as sent_tickers_m1_m2
                     , sum(case when sbt.ew_sent_z < -1 and sbt.ew_sent_z >= -2 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.ew_sent_z < -1 and sbt.ew_sent_z >= -2 then 1 else 0 end) as m1_m2_avg_return
                     , sum(case when sbt.ew_sent_z <= 1 and sbt.ew_sent_z >= -1  then 1 else 0 end) as sent_tickers_m1_p1
                     , sum(case when sbt.ew_sent_z <= 1 and sbt.ew_sent_z >= -1 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.ew_sent_z <= 1 and sbt.ew_sent_z >= -1 then 1 else 0 end) as m1_p1_avg_return
                     , sum(case when sbt.ew_sent_z > 1 and sbt.ew_sent_z <= 2 then 1 else 0 end) as sent_tickers_p1_p2
                     , sum(case when sbt.ew_sent_z > 1 and sbt.ew_sent_z <= 2 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.ew_sent_z > 1 and sbt.ew_sent_z <= 2 then 1 else 0 end) as p1_p2_avg_return
                     , sum(case when sbt.ew_sent_z > 2 then 1 else 0 end) as sent_tickers_p2
                     , sum(case when sbt.ew_sent_z > 2 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.ew_sent_z > 1 and sbt.ew_sent_z > 2 then 1 else 0 end) as p2_avg_return
                     , sum(case when sbt.vol < 5 then 1 else 0 end) as sent_tickers_lt5
                     , sum(case when sbt.vol < 5 then qbt.close_price / qbt.open_price - 1 else 0 end) / sum(case when sbt.vol < 5 then 1 else 0 end) as lt5_avg_return
                     , sum(1) as sentiment_tickers_universe
                     , sum(qbt.close_price / qbt.open_price - 1) / sum(1) sentiment_universe_avg_return
                     From pubv2.QUOTE_BY_TICKER as qbt
                     Join (
                     Select Date(FROM_UNIXTIME(bs.bucket_ts/1000)) as center_date, TIME(FROM_UNIXTIME(bs.bucket_ts/1000)) as center_time
                     , t.value as ticker
                     , bs.ew_sent_z
                     , bs.volume as vol
                     From tm_allinone.TERM as t
                     Left Outer Join tm_allinone.BUCKETED_SCORE as bs
                     On bs.term_id = t.id and bs.job_id = 21
                     where t.type = 2
                     ) as sbt
                     On sbt.ticker = qbt.ticker and qbt.center_date = sbt.center_date and sbt.center_time = '09:10:00'
                     Where qbt.center_date between ",
                    "'",date1,"'",
                    "and",
                    "'",date2,"'",
                    "Group By qbt.center_date
                    Order By qbt.center_date asc;
                    ")#change the date here to get the sma data from database
  smadat <- sqlQuery(ch, query_sma)
    
  fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\sma-RESULTS1.5from",date1,"to",date2,".csv", sep="")
  write.table(smadat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
 
odbcCloseAll()

#########################################################
#import data from SPY@yahoo

library(quantmod)
library(xts)
library(moments)

spydat <- getSymbols("SPY", src="yahoo", auto.assign=FALSE ,from = date1)#change the date here to get spy data from yahoo finance
spydat <- data.frame(spydat)
spydat$center_date <- rownames(spydat)
spydat$center_date <- as.Date(spydat$center_date)

fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\spy-RESULTS1.5from",date1,"to",date2,".csv", sep="")
write.table(spydat, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")

#########################################################
#clean the data and introduce open to close ratio
library(reshape)
a=NULL
count=1
for(i in 1:nrow(smadat)){
  if(smadat$sent_tickers_m2[i]==0||smadat$sent_tickers_p2[i]==0){
    a[count] = i
    count = count+1
  }
}

if(!is.null(a)){
  smadat <- smadat[-1*a,]
}

spydat["o2c_SPY"] <- NA 
for (i in 1:nrow(spydat)) {  
  spydat$o2c_SPY[i] = spydat$SPY.Close[i]/spydat$SPY.Open[i]-1
}

newList <- list("sma" = smadat, "spy" = spydat)

return(newList)

}