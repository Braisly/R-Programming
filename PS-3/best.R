best  <-  function(state,  outcome)  {
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
    
  
  ##  Return  hospital  name  in  that  state  with  lowest  30-day  death  && min(data[,pos])
  data<-data[which(data[,7]==state),]
  hospital<-data[which.min(data[,pos]),2]
  
  sHospital<-order(hospital)
  
  ##  rate
  
  return(hospital[sHospital])
}