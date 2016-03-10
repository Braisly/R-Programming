source("complete.R")


corr <- function(directory,threshold = 0){
  
  data<-complete(directory)
  corFiles<-data[which(data$nobs>threshold),]$id
  
  
  filesDirectory<-paste(getwd(),directory,sep="/")
  allFiles<-list.files(filesDirectory)
  data<-vector(mode = "numeric", length = length(corFiles))
  #data<-numeric()
  
  for(i in corFiles){
    
    dataFile<-read.csv(file=paste(filesDirectory,allFiles[i],sep="/"),head=TRUE,sep=",")
    dataFile<-na.omit(dataFile)
    data[i]<- cor(dataFile[,2], dataFile[,3]) 
  }
  
  return (data[(!is.na(data)) & (data!=0)])

  #return(length(corFiles))
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

