pollutantmean <- function(directory,pollutant,id=1:332){
  
  filesDirectory<-paste(getwd(),directory,sep="/")
  allFiles<-list.files(filesDirectory)
  
  
  data<-vector(mode = "list", length = length(id))
  
  for(i in id)
    data[[i]]<-read.csv(file=paste(filesDirectory,allFiles[i],sep="/"),head=TRUE,sep=",")
  
  
  allData<-do.call(rbind, data)
  

  if(pollutant=="sulfate")
    average<-mean(allData$sulfate,na.rm=TRUE)
  else
    if(pollutant=="nitrate")
      average<-mean(allData$nitrate,na.rm=TRUE)
  
  return (average)
}


pollutantmean("specdata", "nitrate")
