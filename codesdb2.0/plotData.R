plotData <- function(pt){
  partemp <- pt
  
  
  #########################################################
  # plot the acc return for m2, p2 and spy
  attach(partemp)
  date <- center_date
  
  #par(mfrow=c(2,1))
  opar <- par(no.readonly=TRUE)
  
  #plot of rate of s-score changes
  par(lwd=2, cex=1.5, font.lab=2)
  plot(date, Negative_Rate, ylim = c(-0.1,0.1),  main="|S-Score|>2", xlab="DATE", ylab="RETURN RATES")
  lines(date, Negative_Rate, type="l", lty=1, col="red")
  par(new=TRUE)
  #plot(date, Positive_Rate, ylim = c(-0.1,0.1))
  lines(date, Positive_Rate, type="l", lty=1, col="green")
  par(new=TRUE)
  #plot(date, Spy_Rate, ylim = c(-0.1,0.1))
  lines(date, Spy_Rate, type="l", lty=1, col="blue")
  abline(h=c(30), lwd=1.5, lty=2, col="gray")
  library(Hmisc)
  minor.tick(nx=3, ny=3, tick.ratio=0.5)
  legend("topleft", inset=.05, title="return types", c("Negative_Rate","Positive_Rate","Spy_Rate"), lty=c(1,1,1), col=c("red","green","blue"))
  
  #plot of s-score return
  par(lwd=2, cex=1.5, font.lab=2)
  plot(date, m2_avg_return,  main="|S-Score|>2", xlab="DATE", ylab="RETURNs")
  lines(date, m2_avg_return, type="l", lty=1, col="red")
  par(new=TRUE)
  plot(date, p2_avg_return)
  lines(date, p2_avg_return, type="l", lty=1, col="green")
  par(new=TRUE)
  plot(date, o2c_SPY)
  lines(date, o2c_SPY, type="l", lty=1, col="blue")
  abline(h=c(30), lwd=1.5, lty=2, col="gray")
  library(Hmisc)
  minor.tick(nx=3, ny=3, tick.ratio=0.5)
  legend("topleft", inset=.05, title="return types", c("m2_avg_return","p2_avg_return","o2c_SPY"), lty=c(1,1,1), col=c("red","green","blue"))
  
  #plot of accumulative return
  par(lwd=2, cex=1.5, font.lab=2)
  plot(date, Cumulative_Negative, ylim = c(90,110), main="|S-Score|>2", xlab="DATE", ylab="CUMULATIVE RETURN")
  lines(date, Cumulative_Negative, type="l", lty=1, col="red")
  par(new=TRUE)
  plot(date, Cumulative_Positive, ylim = c(90,110))
  lines(date, Cumulative_Positive, type="l", lty=1, col="green")
  par(new=TRUE)
  plot(date, Cumulative_Spy, ylim = c(90,110))
  lines(date, Cumulative_Spy, type="l", lty=1, col="blue")
  abline(h=c(30), lwd=1.5, lty=2, col="gray")
  library(Hmisc)
  minor.tick(nx=3, ny=3, tick.ratio=0.5)
  legend("topleft", inset=.05, title="return types", c("m2","p2","spy"), lty=c(1,1,1), col=c("red","green","blue"))
  
  #plot of cumulative return gap
  plot(date, Spy_Negative, ylim = c(-0.2,0.2), main="|S-Score|>2", xlab="DATE", ylab="CUMULATIVE RETURN gap")
  lines(date, Spy_Negative, type="l", lty=1, col="red")
  par(new=TRUE)
  plot(date, Positive_Spy, ylim = c(-0.2,0.2))
  lines(date, Positive_Spy, type="l", lty=1, col="green")
  par(new=TRUE)
  plot(date, Positive_Negative, ylim = c(-0.2,0.2))
  lines(date, Positive_Negative, type="l", lty=1, col="blue")
  abline(h=c(30), lwd=1.5, lty=2, col="gray")
  library(Hmisc)
  minor.tick(nx=3, ny=3, tick.ratio=0.5)
  legend("bottomleft", inset=.05, title="return types", c("Spy_Negative","Positive_Spy","Positive_Negative"), lty=c(1,1,1), col=c("red","green","blue"))
  
  #plot of (PS-SPY)/(SPY-NS)
  plot(date, PS2SN, ylim = c(-100,100), main="|S-Score|>2", xlab="DATE", ylab="P-S/S-N")
  lines(date, PS2SN, type="l", lty=1, col="black")
  
  detach(partemp)
  
}