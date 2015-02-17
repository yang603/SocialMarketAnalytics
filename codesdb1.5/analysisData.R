analysisData <- function(date1, date2=Sys.Date()){
  #######################################################
  #create a folder 
  mainDir <- "C:\\Users\\Ethan\\Documents\\mydata"
  subDir <- "results"
  #check if the data is already available.
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    cat("subDir exists in mainDir and is a directory")
  } else if (file.exists(paste(mainDir, subDir, sep = "/", collapse = "/"))) {
    cat("subDir exists in mainDir but is a file")
    # you will probably want to handle this separately
  } else {
    cat("subDir does not exist in mainDir - creating")
    dir.create(file.path(mainDir, subDir))
  }
  
  if (file.exists(paste(mainDir, subDir, "/", sep = "/", collapse = "/"))) {
    # By this point, the directory either existed or has been successfully created
    setwd(file.path(mainDir, subDir))
  } else {
    cat("subDir does not exist")
    # Handle this error as appropriate
  }
  
  nL <- checkData(date1, date2)
  
  pt <- processData(nL, date1, date2) 
  
  results <- list("smadata" = nL$sma, "spydata" = nL$spy, "output" = pt)
  
  return(results)
  #########################################################
}