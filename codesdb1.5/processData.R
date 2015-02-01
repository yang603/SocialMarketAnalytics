processData <- function(newList, date1, date2){

  #Process the data sheet and come up with partemp 
  tempsma <- newList$sma
  tempspy <- newList$spy
  totaltemp <- merge(tempsma, tempspy, by = "center_date", all.x=TRUE)
  partemp <- subset(totaltemp, select=c("center_date", "sent_tickers_m2","m2_avg_return","sent_tickers_p2","p2_avg_return","o2c_SPY"))
  
  partemp["Cumulative_Negative"] <- NA
  partemp["Negative_S_Score"] <- NA
  partemp["Cumulative_Positive"] <- NA
  partemp["Positive_S_Score"] <- NA
  partemp["Cumulative_Spy"] <- NA
  partemp["SPY"] <- NA
  
  partemp["Positive_Spy"] <- NA
  partemp["Spy_Negative"] <- NA
  partemp["Positive_Negative"] <- NA
  partemp["PS2SN"] <- NA
  
  partemp["Positive_Rate"] <- NA
  partemp["Negative_Rate"] <- NA
  partemp["Spy_Rate"] <- NA
  
  #partemp$m2_avg_return<-as.numeric(as.character(partemp$m2_avg_return))
  #partemp$p2_avg_return<-as.numeric(as.character(partemp$p2_avg_return))
  
  
  for (i in 1:nrow(partemp)) {
    if(i==1){
      partemp$Negative_S_Score[i] = partemp$m2_avg_return[i]
      partemp$Positive_S_Score[i] = partemp$p2_avg_return[i]
      partemp$SPY[i] = partemp$o2c_SPY[i]
      
      partemp$Cumulative_Negative[i] = 100*(1+partemp$m2_avg_return[i])
      partemp$Cumulative_Positive[i] = 100*(1+partemp$p2_avg_return[i])
      partemp$Cumulative_Spy[i] = 100*(1+partemp$o2c_SPY[i])
      
      partemp$Positive_Spy[i] = partemp$Positive_S_Score[i] - partemp$SPY[i]
      partemp$Spy_Negative[i] = partemp$SPY[i] - partemp$Negative_S_Score[i]
      partemp$Positive_Negative[i] = partemp$Positive_S_Score[i] - partemp$Negative_S_Score[i]
      
      partemp$PS2SN[i] = partemp$Positive_Spy[i]/partemp$Spy_Negative[i]
      
      partemp$Positive_Rate[i]=0
      partemp$Negative_Rate[i]=0
      partemp$Spy_Rate[i]=0
    }else{
      partemp$Cumulative_Negative[i] = partemp$Cumulative_Negative[i-1]*(1+partemp$m2_avg_return[i])
      partemp$Cumulative_Positive[i] = partemp$Cumulative_Positive[i-1]*(1+partemp$p2_avg_return[i])
      partemp$Cumulative_Spy[i] = partemp$Cumulative_Spy[i-1]*(1+partemp$o2c[i])
      
      partemp$Negative_S_Score[i] = partemp$Cumulative_Negative[i]/100 - 1
      partemp$Positive_S_Score[i] = partemp$Cumulative_Positive[i]/100 - 1
      partemp$SPY[i] = partemp$Cumulative_Spy[i]/100 - 1
      
      partemp$Positive_Spy[i] = partemp$Positive_S_Score[i] - partemp$SPY[i]
      partemp$Spy_Negative[i] = partemp$SPY[i] - partemp$Negative_S_Score[i]
      partemp$Positive_Negative[i] = partemp$Positive_S_Score[i] - partemp$Negative_S_Score[i]
      
      partemp$PS2SN[i] = partemp$Positive_Spy[i]/partemp$Spy_Negative[i]
      
      partemp$Positive_Rate[i]=partemp$m2_avg_return[i]-partemp$m2_avg_return[i-1]
      partemp$Negative_Rate[i]=partemp$p2_avg_return[i]-partemp$p2_avg_return[i-1]
      partemp$Spy_Rate[i]=partemp$o2c[i]-partemp$o2c[i-1]
    } 
  }
  
  #########################################################
  #Export the data sheet partemp into fileout
  fileout <- paste("C://Users//Ethan//Documents//mydata//results//return1.5from",date1,"to",date2,".csv", sep="")
  write.table(partemp, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
  
  return(partemp)
}