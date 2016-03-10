complete <- function(directory,id=1:332){
  
  filesDirectory<-paste(getwd(),directory,sep="/")
  allFiles<-list.files(filesDirectory)
  
  
  data<-vector(mode = "list", length = length(id))
  a<-1
  for(i in id){
    dataFile<-read.csv(file=paste(filesDirectory,allFiles[i],sep="/"),head=TRUE,sep=",")
    nComplete<-nrow(na.omit(dataFile))
    
    data[[a]] <- data.frame(dataFile$ID[1], nComplete) 
    a<-a+1
  }
  
  allData<-do.call(rbind, data)
  names(allData)<-c("id","nobs")
  #allData<-allData[which((allData$nobs!=0)),]
  
  
  return (allData)
}


set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, 2])