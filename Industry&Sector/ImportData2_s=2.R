#importData <- function(date1, date2){
  #######################################################
  #import the sma data from database system
    
  date1 <- "2014-10-01"#start time
  date2 <- "2015-01-01"#end time
  
  
  library("RODBC")
  
  options(digits=4)
  
  odbcCloseAll()
  ch = odbcConnect("Falcon_3326")
  
  query <- paste("SET time_zone='US/Eastern';")
  sqlQuery(ch, query)
  
  query <- paste("SET Transaction Isolation Level Read Uncommitted;")
  sqlQuery(ch, query)
    
  query_industry <- paste("
                          select count(*), Industry from(
                          Select t.value as Ticker, t.description as Company, s.name as Sector, i.name as Industry
                          From tm_allinone.TERM as t 
                              Left Join tm_allinone.TERM_SECTOR as ts 
                                  On t.id = ts.term_id and ts.scheme = 2 
                              Left Join tm_allinone.SECTOR as s 
                                  On ts.sector_id = s.id 
                              Left Join tm_allinone.TERM_INDUSTRY as ti 
                                  On t.id = ti.term_id and ti.scheme = 2 
                              Left Join tm_allinone.INDUSTRY as i 
                                  On ti.industry_id = i.id 
                          Order By t.Value asc) a where Industry is not null
                          group by Industry
                          order by count(*) DESC
                          limit 10
                          ")
  
  group_industry <- sqlQuery(ch, query_industry)
    
  query_sector <- paste("
                          select count(*), Sector from(
                          Select t.value as Ticker, t.description as Company, s.name as Sector, i.name as Industry
                          From tm_allinone.TERM as t 
                              Left Join tm_allinone.TERM_SECTOR as ts 
                                  On t.id = ts.term_id and ts.scheme = 2 
                              Left Join tm_allinone.SECTOR as s 
                                  On ts.sector_id = s.id 
                              Left Join tm_allinone.TERM_INDUSTRY as ti 
                                  On t.id = ti.term_id and ti.scheme = 2 
                              Left Join tm_allinone.INDUSTRY as i 
                                  On ti.industry_id = i.id 
                          Order By t.Value asc) a where Sector is not null
                          group by Sector
                          order by count(*) DESC
                          limit 10
                          ")
    
  group_sector <- sqlQuery(ch, query_sector)  
  
  smadata_industry_origion <- list()
    
  for (i in 1:nrow(group_industry)) {
    industry_name <- as.character(group_industry$Industry[i])
    temp <-paste("'",industry_name,"'",sep="")
    query_sma_industry <- paste("
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
                                 and t.value in (
                                  Select t.value as Ticker
                                  From tm_allinone.TERM as t 
                                      Left Join tm_allinone.TERM_SECTOR as ts 
                                          On t.id = ts.term_id and ts.scheme = 2 
                                      Left Join tm_allinone.SECTOR as s 
                                          On ts.sector_id = s.id 
                                      Left Join tm_allinone.TERM_INDUSTRY as ti 
                                          On t.id = ti.term_id and ti.scheme = 2 
                                      Left Join tm_allinone.INDUSTRY as i 
                                          On ti.industry_id = i.id 
                                  where
                                    i.name = ",temp,"
                                  Order By t.Value asc                  
                                  )
                                 ) as sbt
                                 On sbt.ticker = qbt.ticker and qbt.center_date = sbt.center_date and sbt.center_time = '09:10:00'
                                 Where qbt.center_date between ",
                                  "'",date1,"'",
                                  "and",
                                  "'",date2,"'",
                                  "Group By qbt.center_date
                                Order By qbt.center_date asc;
                                ")#change the date here to get the sma data from database
    smadat <- sqlQuery(ch, query_sma_industry)  
    smadata_industry_origion[[industry_name]] <- smadat
    
  }
  
  smadata_sector_origion <- list()
  
  for (i in 1:nrow(group_sector)) {
    sector_name <- as.character(group_sector$Sector[i])
    temp <-paste("'",sector_name,"'",sep="")
    query_sma_sector <- paste("
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
                                and t.value in (
                                Select t.value as Ticker
                                From tm_allinone.TERM as t 
                                Left Join tm_allinone.TERM_SECTOR as ts 
                                On t.id = ts.term_id and ts.scheme = 2 
                                Left Join tm_allinone.SECTOR as s 
                                On ts.sector_id = s.id 
                                Left Join tm_allinone.TERM_INDUSTRY as ti 
                                On t.id = ti.term_id and ti.scheme = 2 
                                Left Join tm_allinone.INDUSTRY as i 
                                On ti.industry_id = i.id 
                                where
                                s.name = ",temp,"
                                Order By t.Value asc                  
                                )
                                ) as sbt
                                On sbt.ticker = qbt.ticker and qbt.center_date = sbt.center_date and sbt.center_time = '09:10:00'
                                Where qbt.center_date between ",
                                "'",date1,"'",
                                "and",
                                "'",date2,"'",
                                "Group By qbt.center_date
                                Order By qbt.center_date asc;
                                ")#change the date here to get the sma data from database
    smadat <- sqlQuery(ch, query_sma_sector)  
    smadata_sector_origion[[sector_name]] <- smadat
  
  }
  
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
  
  smadata_sector_origion[["UNIVERSE"]] <- smadat
  smadata_industry_origion[["UNIVERSE"]] <- smadat

  odbcCloseAll()
  
  #########################################################
  #import data from SPY@yahoo
  
  library(quantmod)
  library(xts)
  library(moments)
  
  spydat <- getSymbols("SPY", src="yahoo", auto.assign=FALSE ,from = date1, to=date2)#change the date here to get spy data from yahoo finance
  spydat <- data.frame(spydat)
  spydat$center_date <- rownames(spydat)
  spydat$center_date <- as.Date(spydat$center_date)
  
  
  #########################################################
  #clear the data
  
  smadata_industry_clear <- list()
  smadata_sector_clear <- list()
  industry_return <- data.frame()
  sector_return <- data.frame()
  
  for(i in 1:length(smadata_industry_origion)){ 
     temp <- merge(spydat, smadata_industry_origion[[names(smadata_industry_origion)[i]]], by = "center_date", all.x=TRUE)
     temp[is.na(temp)]<-0
     smadata_industry_clear[[names(smadata_industry_origion)[i]]] <- temp
  }
  
  
  for(i in 1:length(smadata_sector_origion)){ 
    temp <- merge(spydat, smadata_sector_origion[[names(smadata_sector_origion)[i]]], by = "center_date", all.x=TRUE)
    temp[is.na(temp)]<-0
    smadata_sector_clear[[names(smadata_sector_origion)[i]]] <- temp
  }
  
  #######################################
  #master the data and calculate the return
  
  industry_return <- smadata_industry_clear[[1]][1]
  industry_return[,"SPY"] <- (smadata_industry_clear[[1]][5]-smadata_industry_clear[[1]][2])/smadata_industry_clear[[1]][5]
  industry_return[,"return_SPY"] <- (smadata_industry_clear[[1]][5]-smadata_industry_clear[[1]][2])/smadata_industry_clear[[1]][5]+1
  for(i in 1:length(smadata_industry_clear)){ 
    industry_return[,names(smadata_industry_clear)[i]] <- smadata_industry_clear[[i]][21]
    industry_return[,paste("return_",names(smadata_industry_clear)[i],sep="")] <- smadata_industry_clear[[i]][21]+1
    industry_return[,paste("positive2_",names(smadata_industry_clear)[i],sep="")] <- smadata_industry_clear[[i]][17]
    industry_return[,paste("positive2_return_",names(smadata_industry_clear)[i],sep="")] <- smadata_industry_clear[[i]][17]+1
    industry_return[,paste("negative2_",names(smadata_industry_clear)[i],sep="")] <- smadata_industry_clear[[i]][9]
    industry_return[,paste("negative2_return_",names(smadata_industry_clear)[i],sep="")] <- smadata_industry_clear[[i]][9]+1
  }
#  industry_return[,"SPY"] <- (smadata_industry_clear[[1]][5]-smadata_industry_clear[[1]][2])/smadata_industry_clear[[1]][5]
#  industry_return[,"return_SPY"] <- (smadata_industry_clear[[1]][5]-smadata_industry_clear[[1]][2])/smadata_industry_clear[[1]][5]+1
  
  sector_return <- smadata_sector_clear[[1]][1]
  sector_return[,"SPY"] <- (smadata_sector_clear[[1]][5] - smadata_sector_clear[[1]][2])/smadata_industry_clear[[1]][5]
  sector_return[,"return_SPY"] <- (smadata_sector_clear[[1]][5] - smadata_sector_clear[[1]][2])/smadata_industry_clear[[1]][5]+1
  for(i in 1:length(smadata_sector_clear)){ 
    sector_return[,names(smadata_sector_clear)[i]] <- smadata_sector_clear[[i]][21]
    sector_return[,paste("return_",names(smadata_sector_clear)[i],sep="")] <- smadata_sector_clear[[i]][21]+1
    sector_return[,paste("positive2_",names(smadata_sector_clear)[i],sep="")] <- smadata_sector_clear[[i]][17]
    sector_return[,paste("positive2_return_",names(smadata_sector_clear)[i],sep="")] <- smadata_sector_clear[[i]][17]+1
    sector_return[,paste("negative2_",names(smadata_sector_clear)[i],sep="")] <- smadata_sector_clear[[i]][9]
    sector_return[,paste("negative2_return_",names(smadata_sector_clear)[i],sep="")] <- smadata_sector_clear[[i]][9]+1
  }
#  sector_return[,"SPY"] <- (smadata_sector_clear[[1]][5] - smadata_sector_clear[[1]][2])/smadata_industry_clear[[1]][5]
#  sector_return[,"return_SPY"] <- (smadata_sector_clear[[1]][5] - smadata_sector_clear[[1]][2])/smadata_industry_clear[[1]][5]+1
  
  for(i in 2:nrow(industry_return)){
    industry_return[i,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69)]=industry_return[i,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69)]*industry_return[i-1,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69)]
  }
  
  for(i in 2:nrow(sector_return)){
    sector_return[i,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63)]<-sector_return[i,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63)]*sector_return[i-1,c(3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63)]
  }
  
  for(i in 1:length(names(smadata_industry_clear))){
    industry_return[,paste("Spread_p_n_",names(smadata_industry_clear)[i],sep="")] = industry_return[,paste("positive2_return_",names(smadata_industry_clear)[i],sep="")] - industry_return[,paste("negative2_return_",names(smadata_industry_clear)[i],sep="")]
    industry_return[,paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep="")] = industry_return[,"return_SPY"] - industry_return[,paste("negative2_return_",names(smadata_industry_clear)[i],sep="")]
    industry_return[,paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep="")] = industry_return[,paste("positive2_return_",names(smadata_industry_clear)[i],sep="")] - industry_return[,"return_SPY"]
  }
  
  for(i in 1:length(names(smadata_sector_clear))){
    sector_return[,paste("Spread_p_n_",names(smadata_sector_clear)[i],sep="")] = sector_return[,paste("positive2_return_",names(smadata_sector_clear)[i],sep="")] - sector_return[,paste("negative2_return_",names(smadata_sector_clear)[i],sep="")]
    sector_return[,paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep="")] = sector_return[,"return_SPY"] - sector_return[,paste("negative2_return_",names(smadata_sector_clear)[i],sep="")]
    sector_return[,paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep="")] = sector_return[,paste("positive2_return_",names(smadata_sector_clear)[i],sep="")] - sector_return[,"return_SPY"]
  }
  
  ##############################################
  #plot the data
  library(reshape)
  library(ggplot2)
  pdf(file= paste('C:/Users/Ethan/Documents/mydata/results/','industryfrom',date1,'to',date2,'.pdf'), width=10, height=6.18)
  par(mfrow=(c(1,2)))
    i=1
    output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
    mydata <- melt(output, id=c("center_date"))
    ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
      geom_line() + 
      scale_colour_manual(values = c("blue","green","red", "black")) +
      theme(legend.position="right") +
      xlab("Date") + ylab("Return") +
      ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
    
    output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
    mydata <- melt(output, id=c("center_date"))
    ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
      geom_line() +  
      scale_colour_manual(values = c("blue","red", "green")) +
      theme(legend.position="right") +
      xlab("Date") + ylab("Return") +
      ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=2
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=3
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=4
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=5
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=6
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=7
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=8
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=9
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=10
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=11
  output <- industry_return[,c("center_date",paste("return_",names(smadata_industry_clear)[i],sep=""),paste("positive2_return_",names(smadata_industry_clear)[i],sep=""),paste("negative2_return_",names(smadata_industry_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- industry_return[,c("center_date",paste("Spread_p_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_industry_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_industry_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_industry_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  dev.off() 
  
  pdf(file= paste('C:/Users/Ethan/Documents/mydata/results/','sectorfrom',date1,'to',date2,'.pdf'), width=10, height=6.18)
  par(mfrow=(c(1,2)))
  i=1
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=2
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=3
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=4
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=5
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=6
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=7
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=8
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=9
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  i=10
  output <- sector_return[,c("center_date",paste("return_",names(smadata_sector_clear)[i],sep=""),paste("positive2_return_",names(smadata_sector_clear)[i],sep=""),paste("negative2_return_",names(smadata_sector_clear)[i],sep=""),"return_SPY")]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() + 
    scale_colour_manual(values = c("blue","green","red", "black")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," CUMULATIVE RETURN\r\n",date1," TO ",date2))
  
  output <- sector_return[,c("center_date",paste("Spread_p_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_spy_n_",names(smadata_sector_clear)[i],sep=""),paste("Spread_p_spy_",names(smadata_sector_clear)[i],sep=""))]
  mydata <- melt(output, id=c("center_date"))
  ggplot(data=mydata, aes(x=center_date, y=value, group=variable, colour=variable)) + 
    geom_line() +  
    scale_colour_manual(values = c("blue","red", "green")) +
    theme(legend.position="right") +
    xlab("Date") + ylab("Return") +
    ggtitle(paste(names(smadata_sector_clear)[i]," SPREAD RETURN\r\n",date1," TO ",date2))
  
  dev.off() 