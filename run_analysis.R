merger <- function(url){
  if(!any(installed.packages()=="dplyr")){
    install.packages("dplyr", dependencies = T)
  }
  if(!file.exists("./data")){
    
    dir.create("./data")
    dlandUnzip(url)
    # The script automatically looks for and validates if the selected directory for the file is in the computer. If it is not, it's added, along with the file
    
  }
  library(dplyr)
  
  path <- file.path(getwd(),"data","UCI HAR Dataset","test","Inertial Signals") 
  path2 <- file.path(getwd(),"data","UCI HAR Dataset","train","Inertial Signals") 
  tpath <- file.path(getwd(),"data","UCI HAR Dataset","test") 
  tpath2 <- file.path(getwd(),"data","UCI HAR Dataset","train") 
  apath <- file.path(getwd(),"data","UCI HAR Dataset")
  ListAct <- as.character(list.files(apath))
  ListTest0 <- as.character(list.files(tpath))
  ListTrain0 <- as.character(list.files(tpath2))
  ListTestPath <- paste(tpath,ListTest0,sep="/")
  ListTrainPath <- paste(tpath2,ListTrain0,sep="/")
  ActPath <- paste(apath,ListAct,sep = "/")
  ListTest <- as.character(list.files(path)) 
  TestPath <- paste(path,ListTest,sep="/")
  ListTrain <- as.character(list.files(path2))
  TrainPath <- paste(path2,ListTrain,sep="/")
  subjtest <- read.table(ListTestPath[2])
  names(subjtest) <- c("subject")
  subjtrain <- read.table(ListTrainPath[2])
  names(subjtrain) <- c("subject")
  feats <- ActPath[2]
  d <- c()
  h <- c()
  #The "path", and "path2" objects retrieve the full "test" and "train" dirs. They should be changed, were the zip file not the same as the one intended for this         #proyect
  t_ <- unlist(read.table(TestPath[1]))
  tr_ <- unlist(read.table(TrainPath[1]))
  act_ <- (read.table(ActPath[1]))
  act_ <- act_$V2
  names(act_) <- c("Activity")
  #Using dplyr to make a a tidy dataset for the "activity" labels
  testxtest <- (read.table(ListTestPath[3]))
  testytest <- (read.table(ListTestPath[4]))
  trainxtest <- (read.table(ListTrainPath[3]))
  trainytest <- (read.table(ListTrainPath[4]))
  traindf <- data.frame(n = 1:length(tr_))
  testdf <- data.frame(n = 1:length(t_))
  feats_ <- read.table(feats)
  feats_ <- feats_$V2
  names(testxtest) <- feats_
  names(testytest) <- feats_
  names(trainytest) <- feats_
  names(trainxtest) <- feats_
  testcomp <- cbind(subjtest,testxtest,testytest)
  traincomp <- cbind(subjtrain,trainxtest,trainytest)
  #Both "traindf" and "testdf"  were initialized with the same first column for the sake of simplicity with the merging. It will get a sample of the dt and assume them   #all equal, so consider that for other files
  
  for (i in 1:length(ListTest)) {
    d <- append(d,paste("Test","-",i,sep = ""),after = length(d))
    h <- append(h,paste("Train","-",i,sep = ""),after = length(h))
    #the column name vector is inicialized here
  }
  for(i in 1:length(ListTest)){
    
    datatest <- unlist(read.table(TestPath[i]))
    testdf[[d[i]]] <- as.numeric(datatest)
    
  }
  for(i in 1:length(ListTest)){
    
    datatrain <- unlist(read.table(TrainPath[i]))
    traindf[[h[i]]] <- as.numeric(datatrain)
  }
  datacomp <- cbind(testcomp,traincomp)
  
  #The read.table commands had to be unlisted in order to have the full list of data without any column interruptions
  mergedf <- merge(traindf,testdf, by.x = "n", by.y = "n")
  #the mergedf becomes a public variable for further usages down the line. It also merges both sets using "n" as identifier. The data frame merges ALL rows, so it will
  # have the amount of rows of the biggest set.
  
    meanVs <- grep('mean\\(\\)', feats_, value=TRUE)
    stdVs <- grep('std\\(\\)', feats_, value=TRUE)
    reqdCs <- c(rbind(meanVs, stdVs))
    reqdCs <- c('subject', 'activity', reqdCs)
    reqdDt <- datacomp[,reqdCs]
    reqData <- reqdDt
    rm(dataComplete)
    
    actLabels <- read.table(ActPath[1])
    
    reqdData$activity <- factor(as.factor(reqdData$activity), 
                                levels=actLabels$V1, 
                                labels=actLabels$V2)
    
    cols2clean <- names(reqdData)
    cols2Clean <- gsub('fBodyBody', 'fBody', cols2Clean)
    cols2Clean <- gsub('^t', 'time\\.', cols2Clean)
    cols2Clean <- gsub('^f', 'freq\\.', cols2Clean)
    els2Change <- grepl('-mean\\(\\)-', cols2Clean)
    cols2Clean[els2Change] <- paste0(
      gsub('mean\\(\\)-', '', cols2Clean[els2Change]), 
      '.mean'
    )  
    cols2Clean <- gsub('-mean\\(\\)', '.mean', cols2Clean)
    
    els2Change <- grepl('-std\\(\\)-', cols2Clean)
    
    cols2Clean[els2Change] <- paste0(
      gsub('std\\(\\)-', '', cols2Clean[els2Change]), 
      '.std'
    )  
    
    #several details within the set are changed, regarding the names of several different measurements
    
    
    cols2Clean <- gsub('-std\\(\\)', '.std', cols2Clean)
    cols2Clean <- gsub('-', '.', cols2Clean)
    names(reqdData) <- cols2Clean
    
    
    
    meanBySubAct <- reqdData %>% 
      group_by(subject, activity) %>% 
      summarise_all(mean)
    if(!file.exists("Res5.txt")){
      outF <- 'Res5.txt'
      write.table(meanBySubAct, file = outF, row.names = FALSE)
    }
    
    #If the file doesn´t exist, it is created
    
   

}



dlandUnzip <- function(url_){
  download.file(url_,"./data/dataset.zip")
  unzip("./data/dataset.zip",exdir = "./data")
}
#fairly straightforward code used to download and unzip a zip file


