# GettingAndCleaningData
##SCRIPTS
##Download dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="C:\\Users\\Dimitris\\Rdata\\Dataset.zip")

##unzip dataset
unzip(zipfile="C:\\Users\\Dimitris\\Rdata\\Dataset.zip",exdir="C:\\Users\\Dimitris\\Rdata")

##load libraries
library(dplyr)
library(data.table)
library(tidyr)

##read subject, activity and data files
filesPath <- "C:\\Users\\Dimitris\\Rdata\\UCI HAR Dataset"
dataSubjTr <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjTs  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))
dataActTr <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActTs  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))
dataTr <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTs  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

## 1. Merge the training and the test sets to create one data set. For the merge, row binding rbing() is used. Rename the variables "subject" and "activityNum)
merDataSubj <- rbind(dataSubjTr, dataSubjTs)
setnames(merDataSubj, "V1", "subject")
merDataAct<- rbind(dataActTr, dataActTs)
setnames(merDataAct, "V1", "activityNum")

## Merge data files
dataTable <- rbind(dataTr, dataTs)

## Read features.txt and use it to name the variables
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

## Read activity_labels.txt and use it to name the columns
activityLbl<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLbl, names(activityLbl), c("activityNum","activityName"))
setnames(merDataAct, "V1", "activityNum")

## Merge columns
merDataSubjAct<- cbind(merDataSubj, merDataAct)
dataTable <- cbind(merDataSubjAct, dataTable)

## 2. Extract only the measurements on the mean and standard deviation for each measurement. Read features.txt and extract only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) 

## Extract only the measurements on the mean and standard deviation and add 'subject','activityNum'
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##3. Use descriptive activity names to name the activities in the data set.  Put the activity name into the dataTable
dataTable <- merge(activityLbl, dataTable , by="activityNum", all.x=TRUE)

## Create a dataTable with means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

## 4. Appropriately label the data set with descriptive variable names.
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

## 5. From the data set in step 4, create a second, independent tidy data set
# with the average of each variable for each activity and each subject.
write.table(dataTable, "TidyData.txt", row.name=FALSE)
