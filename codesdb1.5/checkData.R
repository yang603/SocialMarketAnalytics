checkData <- function(date1, date2){
  date1 = time1
  date2 = time2
  if(date2 == Sys.Date()){
    newList <- importData(date1, date2)
  }else{   
    smafileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\sma-RESULTS1.5from",date1,"to",date2,".csv", sep="")
    spyfileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\spy-RESULTS1.5from",date1,"to",date2,".csv", sep="")

    if (file.exists(smafileout)&&file.exists(spyfileout)){
      smadata <- read.csv(gsub(" ","", smafileout, fixed=TRUE), header = TRUE, sep = ",",stringsAsFactors=FALSE)
      spydata <- read.csv(gsub(" ","", spyfileout, fixed=TRUE), header = TRUE, sep = ",",stringsAsFactors=FALSE)
      
      
      smadata$center_date<-as.Date(as.character(smadata$center_date))
      spydata$center_date<-as.Date(as.character(smadata$center_date))
      
      smadate2=-1
      spydate2=-1
      for(i in 1:nrow(smadata)){
        if(smadate2==-1){
          if(as.Date(date2) == smadata$center_date[i]){
            smadate2 = i
          }else if((i>1)&&(i!=nrow(smadata))&&(as.Date(date2)>smadata$center_date[i-1])&&(as.Date(date2)<smadata$center_date[i+1])){
            smadate2 = i 
          }else if(i==nrow(smadata)){
            smadate2=i
          }
        }
        
        if(spydate2==-1){
          if(as.Date(date2) == spydata$center_date[i]){
            spydate2 = i
          }else if((i>1)&&(i!=nrow(spydata))&&(as.Date(date2)>spydata$center_date[i-1])&&(as.Date(date2)<spydata$center_date[i+1])){
            spydate2 = i 
          }else if(i==nrow(spydata)){
            spydate2=i
          }      
        }
      }
      
      sma <- smadata[1:smadate2,]
      spy <- spydata[1:spydate2,]
      
      newList <- list("sma" = sma, "spy" = spy)
      
    }else{
      newList <- importData(date1, Sys.Date())
      smadata <- newList$sma
      spydata <- newList$spy
      
      smadate2=-1
      spydate2=-1
      for(i in 1:nrow(smadata)){
        if(smadate2==-1){
          if(as.Date(date2) == smadata$center_date[i]){
            smadate2 = i
          }else if((i>1)&&(i!=nrow(smadata))&&(as.Date(date2)>smadata$center_date[i-1])&&(as.Date(date2)<smadata$center_date[i+1])){
            smadate2 = i 
          }else if(i==nrow(smadata)){
            smadate2=i
          }
        }
        
        if(spydate2==-1){
          if(as.Date(date2) == spydata$center_date[i]){
            spydate2 = i
          }else if((i>1)&&(i!=nrow(spydata))&&(as.Date(date2)>spydata$center_date[i-1])&&(as.Date(date2)<spydata$center_date[i+1])){
            spydate2 = i 
          }else if(i==nrow(spydata)){
            spydate2=i
          }      
        }
      }
      
      sma <- smadata[1:smadate2,]
      spy <- spydata[1:spydate2,]
      
      newList <- list("sma" = sma, "spy" = spy)
      
    }
    
  }
  
  fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\sma-RESULTS1.5from",date1,"to",date2,".csv", sep="")
  write.table(newList$sma, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
  fileout <- paste("C:\\Users\\Ethan\\Documents\\mydata\\results\\spy-RESULTS1.5from",date1,"to",date2,".csv", sep="")
  write.table(newList$spy, file=fileout, col.names = TRUE, row.names = FALSE, quote = FALSE, sep=",")
  
  return(newList)
}