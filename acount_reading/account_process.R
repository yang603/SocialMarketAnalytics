
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
  SELECT distinct ticker FROM pubv2.QUOTE_BY_TICKER;
                        ")
tickers <- sqlQuery(ch, query_tic)

#save all the tickers based on sector and industry
 

# read in twitter in csv format

smafileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\twitter_info.csv", sep="")
smadata <- read.csv(gsub(" ","", smafileout, fixed=TRUE), header = TRUE, sep = ",",stringsAsFactors=FALSE)
conclusion <- smadata[c("posted_date","posting_account","posting_account_id")]

conclusion["tic"] = ""
conclusion["num_tic"] = 0
conclusion["ishttp"] = 0

array <- tickers$ticker
for(i in array){
  count <- 0
  temp0 <- ""
  ticker1 <- paste("\\$",i,"+$",sep="")
  ticker2 <- paste("\\$",i," ",sep="")
  num1 <- grep(ticker1,smadata$contents)
  num2 <- grep(ticker2,smadata$contents)
  num <- unique(c(num1,num2))
  for (j in num){
    temp0 <- conclusion$tic[j]
    temp1 <- paste(temp0,i,sep="$")
    conclusion$tic[j] <- temp1
    temp0 <- conclusion$num_tic[j]
    temp1 <- temp0+1
    conclusion$num_tic[j] <- temp1
  }
}

num <- grep("http://t.co/",smadata$contents)
conclusion$ishttp[num] = 1
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
  
