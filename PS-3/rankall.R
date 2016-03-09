
rankall  <-  function(outcome,  num  =  "best")  {
  
  data  <-  read.csv("data/outcome-of-care-measures.csv",  colClasses  =  "character")
  
  data<-na.omit(data)
  states<-unique(data[,7])
  states<-states[order(states)]
  rHospital<- data.frame(states,NA)
    
  typeOutcome<-c("heart attack","heart failure","pneumonia")
  
  ##  Check  that  state  and  outcome  are  valid
  
  try(
      if(! outcome %in% typeOutcome)
        stop("Invalid outcome!")
  )
  
  if(outcome==typeOutcome[1])
    pos<-11
  else
    if(outcome==typeOutcome[2])
      pos<-17
    else
      if(outcome==typeOutcome[3])
        pos<-23
  
  #List of data frames per State
  dataGroup<-split(data,data[,7])
  
  ##  For  each  state,  find  the  hospital  of  the  given  rank
  
  if(num=="worst")
    rHospital[,2]<-sapply(dataGroup,function(x) x[which.max(x[,pos]),2])
  else{
    if(num=="best")
      rHospital[,2]<-sapply(dataGroup,function(x) x[which.min(x[,pos]),2])
    else
      rHospital[,2]<-sapply(dataGroup,function(x) x[order(x[,pos],x[,2]),2][num])
  }
  
  
  ##  Return  a  data  frame  with  the  hospital  names  and  the
  ##  (abbreviated)  state  name
  
  names(rHospital)<-c("state","hospital")
  
  return(rHospital)

}

head(rankall("heart attack",  20),  10)
tail(rankall("pneumonia",  "worst"),  3)
tail(rankall("heart failure"),  10)
