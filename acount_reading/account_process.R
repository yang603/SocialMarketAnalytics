
library("RODBC")

options(digits=4)
#save all the tickers in the tickers
odbcCloseAll()
ch = odbcConnect("Falcon_3326")
query <- paste("SET time_zone='US/Eastern';")
sqlQuery(ch, query)
query <- paste("SET Transaction Isolation Level Read Uncommitted;")
sqlQuery(ch, query)
query_tic <- paste("
  SELECT distinct value as ticker FROM tm_allinone.TERM;
                        ")
tickers <- sqlQuery(ch, query_tic)

#save all the tickers based on sector and industry
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

query_sector_industry <- paste("
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
                          Order By t.Value asc
                          ")

group_sector_industry <- sqlQuery(ch, query_sector_industry)



# read in twitter in csv format

smafileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\twitter_info.csv", sep="")
smadata <- read.csv(gsub(" ","", smafileout, fixed=TRUE), header = TRUE, sep = ",",stringsAsFactors=FALSE)
conclusion <- smadata[c("posted_date","posting_account","posting_account_id")]

conclusion["tic"] = ""
conclusion["num_tic"] = 0
conclusion["ishttp"] = 0

for(i in group_sector[2]){
  print(i)
  conclusion[paste("num_",i,sep="")] = 0
}

#update the statistics profile for individual twitter for corresponding account

for(i in 1:range(length(group_sector_industry$Ticker))){
  count <- 0
  temp0 <- ""
  ticker1 <- paste("\\$",group_sector_industry$Ticker[i],"+$",sep="")
  ticker2 <- paste("\\$",group_sector_industry$Ticker[i]," ",sep="")
  num1 <- grep(ticker1,smadata$contents)
  num2 <- grep(ticker2,smadata$contents)
  num <- unique(c(num1,num2))
  for (j in num){
    temp0 <- conclusion$tic[j]
    temp1 <- paste(temp0,group_sector_industry$Ticker[i],sep="$")
    conclusion$tic[j] <- temp1
    temp0 <- conclusion$num_tic[j]
    temp1 <- temp0+1
    conclusion$num_tic[j] <- temp1
    if(!is.na(group_sector_industry$Sector[i])){     
      temp0 <- conclusion[,paste("num_",group_sector_industry$Sector[i],sep="")][j]
      temp1 <- temp0+1
      conclusion[,paste("num_",group_sector_industry$Sector[i],sep="")][j] <- temp1
    }
  }
}



##Attention to the split!!!
conclusion$tic

conclusion["source"] = ""
for(i in 1:length(smadata$source)){
  if(is.na(smadata$source[i])){
    conclusion$source[i]=8
  }else if(smadata$source[i]=="http://twitter.com/download/android"){
    conclusion$source[i]=1
  }else if((smadata$source[i]=="http://twitter.com/download/iphone")||(smadata$source[i]=="http://twitter.com/#!/download/iphone")){
    conclusion$source[i]=2
  }else if(smadata$source[i]=="http://blackberry.com/twitter"){
    conclusion$source[i]=3
  }else if(smadata$source[i]=="http://twitter.com/#!/download/ipad"){
    conclusion$source[i]=4
  }else if(smadata$source[i]=="http://twitter.com"){
    conclusion$source[i]=5
  }else if(smadata$source[i]=="http://www.tweetdeck.com"){
    conclusion$source[i]=6
  }else if((smadata$source[i]=="http://stocktwits.com")||(smadata$source[i]=="http://seekingalpha.com")||(smadata$source[i]=="http://www.nasdaq.com/")||(smadata$source[i]=="http://www.freestockcharts.com")||(smadata$source[i]=="http://stocksignaling.com/twitterapp/")){
    conclusion$source[i]=7
  }else{
    conclusion$source[i]=8
  }
}
  
