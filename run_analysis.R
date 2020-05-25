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
  t_ <- unlist(read.table(TestPath[1]))
  tr_ <- unlist(read.table(TrainPath[1]))
  traindf <- data.frame(n = 1:length(tr_))
  testdf <- data.frame(n = 1:length(t_))
  for (i in 1:length(ListTest)) {
    d <- append(d,paste("Test","-",i,sep = ""),after = length(d))
    h <- append(h,paste("Train","-",i,sep = ""),after = length(h))
  }
  for(i in 1:length(ListTest)){
    
    datatest <- unlist(read.table(TestPath[i]))
    testdf[[d[i]]] <- as.numeric(datatest)
  }
  for(i in 1:length(ListTest)){
    
    datatrain <- unlist(read.table(TrainPath[i]))
    traindf[[h[i]]] <- as.numeric(datatrain)
  }
  
  mergedf <<- merge(traindf,testdf, by.x = "n", by.y = "n", all = T)
  
  return(mergedf)
}

extracter <- function(url__){
  m <- merger(url__)
  namesM_ <- names(m)
  namesM <- namesM_[-1]
  lenM <- length(namesM)
  Avg <- data.frame(n = 1)
  StdDev <- data.frame(n = 1)
  
  for (i in 1:lenM) {
    namme <- paste("Mean-",namesM[i],"     ",sep = "")
    nasme <- paste("Std.Dev-", namesM[i],sep = "")
    m1 <- mean(m[[namesM[i]]],na.rm = T)
    Avg[[namme]] <- m1
    s1 <- sd(m[[namesM[i]]],na.rm = T)
    StdDev[[nasme]] <- s1
    
  }
  
  if(!file.exists("Avg.txt")){
    write.table(Avg, "Avg.txt", row.names = F)
  }
  if(!file.exists("SD.txt")){
    write.table(StdDev, "SD.txt", row.names = F)
  }
}

dlandUnzip <- function(url_){
  download.file(url_,"./data/dataset.zip")
  unzip("./data/dataset.zip",exdir = "./data")
}
