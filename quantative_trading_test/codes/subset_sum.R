#This function is used to traverse all the combination of record in data to find all combination that sum up to target value
subset_sum <- function(data, target, fromIndex, endIndex, stack, sumInStack, output=list(c())){  
  
  if(sumInStack==target){
    output[length(output)+1]<-list(stack)
    return(output)
  }
  
  for(i in fromIndex:endIndex){
    
    if(i>endIndex){
      break
    }
    
    if((sumInStack + data[i,]$size)<=target){
      tempstack <- stack
      tempstack[length(stack)+1] <- rownames(data[i,])
      sumInStack = sumInStack + data[i,]$size
      
      output<-subset_sum(data, target, i+1, endIndex, tempstack, sumInStack, output)
      
      sumInStack = sumInStack - data[i,]$size
    }
  }
  return(output)
}
