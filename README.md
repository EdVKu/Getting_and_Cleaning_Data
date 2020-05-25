# Getting_and_Cleaning_Data

## Script used
´´´r
merger <- function(url){

  if(!file.exists("./data")){
  
    dir.create("./data")
    dlandUnzip(url)
    
  }
 
  path <- file.path(getwd(),"data","UCI HAR Dataset","test","Inertial Signals")
  path2 <- file.path(getwd(),"data","UCI HAR Dataset","train","Inertial Signals")
  ListTest <- as.character(list.files(path))
  TestPath <- paste(path,ListTest,sep="/")
  ListTrain <- as.character(list.files(path2))
  TrainPath <- paste(path2,ListTrain,sep="/")
  d <- c()
  h <- c()
  traindf <- data.frame(n = 1:7352)
  testdf <- data.frame(n = 1:2947)
  
  for (i in 1:length(ListTest)) {
  
    d <- append(d,paste("Test","-",i,sep = ""),after = length(d))
    h <- append(h,paste("Train","-",i,sep = ""),after = length(h))
    
  }
  
  for(i in 1:length(ListTest)){
    
    datatest <- read.table(TestPath[i])
    testdf[[d[i]]] <- datatest
  }
  for(i in 1:length(ListTest)){
    
    datatrain <- read.table(TrainPath[i])
    traindf[[h[i]]] <- datatrain
  }
  
  mergedf <<- merge(traindf,testdf, by.x = "n", by.y = "n")

}
´´´
