
rankhospital  <-  function(state,  outcome,  num  =  "best")  {
  ##  Read  outcome  data
  
  data  <-  read.csv("data/outcome-of-care-measures.csv",  colClasses  =  "character")
  data<-na.omit(data)
  
  typeOutcome<-c("heart attack","heart failure","pneumonia")
  
  
  
  ##  Check  that  state  and  outcome  are  valid
  try(
    if(length(data[which(data[,7]==state),7])!=0){
      try(if(! outcome %in% typeOutcome)
        stop("Invalid outcome!"))
    }else{
      stop("Invalid State!")
    }
  )
  
  
  if(outcome==typeOutcome[1])
    pos<-11
  else
    if(outcome==typeOutcome[2])
      pos<-17
  else
    if(outcome==typeOutcome[3])
      pos<-23
  
  
  
  ##  Return  hospital  name  in  that  state  with  the  given  rank
  data<-data[which(data[,7]==state),]
  if(num=="worst"){
    hospital<-data[which.max(data[,pos]),2]
    sHospital<-order(hospital)
    hospital<-hospital[sHospital]
  }else{
    if(num=="best")
      num<-1
    sHospital<-order(as.numeric(data[,pos]),data[,2])
    hospital<-data[sHospital,]
    hospital<-hospital[num,2]
  }
      
  
  ## Return  30-day  death  rate
  
  
  return(hospital)
}