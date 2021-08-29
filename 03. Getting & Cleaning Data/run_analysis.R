##Download
  filepath <- "C:/Users/n.taghidoost/Desktop/RFunctions/getdata_projectfiles_UCI HAR Dataset.zip"
  filepath2 <- "C:/Users/n.taghidoost/Desktop/RFunctions"
    if(!file.exists(filepath)) {
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url = url,destfile = filepath,method = "curl")
    }
  unzip(zipfile = filepath,exdir = filepath2)
  setwd("C:/Users/n.taghidoost/Desktop/RFunctions/UCI HAR Dataset")
##-------------------------------------------------------------------------------
  
##Get Data
    
  library(dplyr)
  yTrainActivity<- read.table("./train/Y_train.txt",header =FALSE )
  
  
  yTestActivity<- read.table("./test/Y_test.txt",header =FALSE )

  
  xTestFeature<- read.table("./test/X_test.txt",header =FALSE )
  
  xTrainFeature<- read.table("./train/X_train.txt",header =FALSE )
  
  TestSubject<- read.table("./test/subject_test.txt",header =FALSE )
  
  TrainSubject<- read.table("./train/subject_train.txt",header =FALSE)
##-----------------------------------------------------------
  
##Merge Data
  
  Subject <- rbind(TestSubject,TrainSubject)
  
  Feature <- rbind(xTestFeature,xTrainFeature)
  
  Activity <- rbind(yTestActivity,yTrainActivity)
  
##------------------------------------------------ 
  
##Names  
  names(Subject) <- c("Subject")
  names(Activity) <- c("Activity")
  FeatureName <- read.table("./features.txt" )
  names(Feature) <- FeatureName$V2
##-------------------------------------------------------------

  GeneralData <- cbind(Subject,Activity,Feature)
##----------------------------------------------------
  
##SUbsetting Data to get specific fields
  ActivityName <- read.table("./activity_labels.txt")
  GeneralData2 <- merge(GeneralData,ActivityName,by.x="Activity",by.y ="V1" )
  NamesWithMeanStd <- FeatureName$V2[grep("mean\\(\\)|std\\(\\)",FeatureName$V2)]
  SelectedNames <- c("Subject","V2",as.character(NamesWithMeanStd))
  GeneralData3 <-subset(GeneralData2,select =SelectedNames)
##---------------------------------------------------
  
##Correcting Names  
  GeneralData3 <- rename(GeneralData3,"ActivityName"="V2")
  names(GeneralData3) <- gsub("Gyro","Gyroscope",names(GeneralData3))
  names(GeneralData3) <- gsub("^t","time",names(GeneralData3))
  names(GeneralData3)<-gsub("BodyBody", "Body", names(GeneralData3))
  names(GeneralData3)<-gsub("^f", "frequency", names(GeneralData3))
  names(GeneralData3)<-gsub("Acc", "Accelerometer", names(GeneralData3))
  names(GeneralData3)<-gsub("Mag", "Magnitude", names(GeneralData3))

  ##-----------------------------------------------------------
  
##Creating Tidy Data  
  TidySet <-  GeneralData3 %>% group_by(Subject,ActivityName) %>%  summarise_all(mean)
  
##-------------------------------------------------------------
  
  ##Write into file
  
  write.table(TidySet,"./TidySet.txt")
