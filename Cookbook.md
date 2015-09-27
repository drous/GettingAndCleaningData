---
title: "Cookbook.md"
author: "DR"
date: "Sunday, September 27, 2015"
output: html_document
---

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Here are the data for the project: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following. 
1) Merges the training and the test sets to create one data set.


2) Extracts only the measurements on the mean and standard deviation for each measurement. 
3) Uses descriptive activity names to name the activities in the data set


4) Appropriately labels the data set with descriptive variable names. 


5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

PROJECT STEPS
# DOWNLOADING THE DATA
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="C:\\Users\\Dimitris\\Rdata\\Dataset.zip")

#UNZIP THE DATA
unzip(zipfile="C:\\Users\\Dimitris\\Rdata\\Dataset.zip",exdir="C:\\Users\\Dimitris\\Rdata")

# LOADING LIBRARIES
library(dplyr)
library(data.table)
library(tidyr)

FILES IN THE PROJECT FOLDER: UCI HAR Dataset
There are 2 SUBJECT files, titled subject_test.txt and subject_traind.txt, one at the 'test' folder and one at the 'train' folder respectively
There are 2 ACTIVITY files, titled X_test.txt and X_train.txt, one at the 'test' folder and one at the 'train' folder respectively
There are 2 DATA files, titled titled y_test.txt and y_train.txt, ne at the 'test' folder and one at the 'train' folder respectively
The features.txt files holds the names of the column variables in the data table
The activity_labels.txt file links the class labels with the activity names.

#read subject, activity and data files
filesPath <- "C:\\Users\\Dimitris\\Rdata\\UCI HAR Dataset"
dataSubjTr <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
dataSubjTs  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))
dataActTr <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
dataActTs  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))
dataTr <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
dataTs  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

# 1. Merge the training and the test sets to create one data set.
# For the merge, row binding rbing() is used
# Rename the variables "subject" and "activityNum
merDataSubj <- rbind(dataSubjTr, dataSubjTs)
setnames(merDataSubj, "V1", "subject")
merDataAct<- rbind(dataActTr, dataActTs)
setnames(merDataAct, "V1", "activityNum")

#Merge data files
dataTable <- rbind(dataTr, dataTs)

#Read features.txt and use it to name the variables
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#Read activity_labels.txt and use it to name the columns
activityLbl<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLbl, names(activityLbl), c("activityNum","activityName"))
setnames(merDataAct, "V1", "activityNum")

# Merge columns
merDataSubjAct<- cbind(merDataSubj, merDataAct)
dataTable <- cbind(merDataSubjAct, dataTable)

#2. Extract only the measurements on the mean and standard deviation for each measurement
# Read features.txt and extract only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) 

# Extract only the measurements on the mean and standard deviation 
#and add 'subject','activityNum'
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

#3. Use descriptive activity names to name the activities in the data set
##Unput the activity name into the dataTable
dataTable <- merge(activityLbl, dataTable , by="activityNum", all.x=TRUE)

# Create a dataTable with means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

#4. Appropriately label the data set with descriptive variable names.
Before ** check with: head(str(dataTable),2) **
std(): Standard Deviation
mean(): Mean
Starting with t (^t) is for time and f (^f) is for frequency.
Body: body movement.
Gravity: Acceleration of gravity
Acc: Accelerometer measurement
Gyro: gyroscopic measurements
Jerk: sudden movement acceleration
Mag: magnitude of movement


names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

New labels ** check with: head(str(dataTable),6) **
std --> SD
mean --> MEAN
^t --> time
^f --> frequency
Acc --> Accelerometer
Gyro --> Gyroscope
BodyBody --> Body

# 5. From the data set in step 4, create a second, independent tidy data set
# with the average of each variable for each activity and each subject.
write.table(dataTable, "TidyData.txt", row.name=FALSE)