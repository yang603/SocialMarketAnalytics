
library("RODBC")

options(digits=4)

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



smafileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\twitter_info.csv", sep="")
smadata <- read.csv(gsub(" ","", smafileout, fixed=TRUE), header = TRUE, sep = ",",stringsAsFactors=FALSE)


con <- smadata[c("posted_date","posting_account","posting_account_id")]


conclusion <- cbind(tic = "", con)
conclusion$tic <- as.character(conclusion$tic)
conclusion

array <- tickers$ticker
for(i in array){
  count <- 0
  temp0 <- ""
  ticker <- paste("\\$",i,sep="")
  for (j in grep(ticker,smadata$contents)){
    temp0 <- conclusion$tic[j]
    temp1 <- paste(temp0,i,sep="$")
    conclusion$tic[j] <- temp1
  }
}
##Attention to the split!!!
conclusion$tic
