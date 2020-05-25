# Getting_and_Cleaning_Data


## Functions used
**Merger**

```{r}
merger <- function(url){

  if(!file.exists("./data")){
  
    dir.create("./data")
    dlandUnzip(url)
    # The script automatically looks for and validates if the selected directory for the file is in the computer. If it is not, it's added, along with the file
    
  }
  
 
  path <- file.path(getwd(),"data","UCI HAR Dataset","test","Inertial Signals") 
  path2 <- file.path(getwd(),"data","UCI HAR Dataset","train","Inertial Signals") 
  ListTest <- as.character(list.files(path)) 
  TestPath <- paste(path,ListTest,sep="/")
  ListTrain <- as.character(list.files(path2))
  TrainPath <- paste(path2,ListTrain,sep="/")
  d <- c()
  h <- c()
  #The "path", and "path2" objects retrieve the full "test" and "train" dirs. They should be changed, were the zip file not the same as the one intended for this         #proyect
  t_ <- unlist(read.table(TestPath[1]))
  tr_ <- unlist(read.table(TrainPath[1]))
  traindf <- data.frame(n = 1:length(tr_))
  testdf <- data.frame(n = 1:length(t_))
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
   #The read.table commands had to be unlisted in order to have the full list of data without any column interruptions
  mergedf <<- merge(traindf,testdf, by.x = "n", by.y = "n")
  #the mergedf becomes a public variable for further usages down the line. It also merges both sets using "n" as identifier. The data frame merges ALL rows, so it will
  # have the amount of rows of the biggest set.
}

```

**dlandUnzip**

```{r}
dlandUnzip <- function(url_){
  download.file(url_,"./data/dataset.zip")
  unzip("./data/dataset.zip",exdir = "./data")
}
#fairly straightforward code used to download and unzip a zip file
```

**extracter**

```{r}
extracter <- function(url__){
  m <- merger(url__)
  namesM_ <- names(m)
  namesM <- namesM_[-1]
  
  #the "n" column is removed
  
  lenM <- length(namesM)
  Avg <- data.frame(n = 1)
  StdDev <- data.frame(n = 1)
  
  #both average (mean) and Standard Deviation are one dimensional data frames
  
  for (i in 1:lenM) {
    namme <- paste("Mean-",namesM[i],"     ",sep = "")
    nasme <- paste("Std.Dev-", namesM[i],sep = "")
    m1 <- mean(m[[namesM[i]]],na.rm = T)
    Avg[[namme]] <- m1
    s1 <- sd(m[[namesM[i]]],na.rm = T)
    StdDev[[nasme]] <- s1
  # Here, the average and sd data frames are prepared. Since there is more data in one type of reading than the other, na.rm is used
  }
  
if(!file.exists("Avg.txt")){
  write.table(Avg, "Avg.txt", row.names = F)
  #If a file exists, the write.table does nothing
}
if(!file.exists("SD.txt")){
    write.table(StdDev, "SD.txt", row.names = F)
    #If a file exists, the write.table does nothing
  }
}
```
