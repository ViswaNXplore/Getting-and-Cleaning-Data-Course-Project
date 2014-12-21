##########################################################################################################

## Coursera Getting and Cleaning Data Course Project
## ViswaNXplore

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################


# Clean up workspace
rm(list=ls())

#set the URL to locate your data folder
data_URL <- file.path("./UCI HAR Dataset")

# 1. Merge the training and the test sets to create one data set.



# Read in the data from files
features     = read.table(file.path(data_URL, "features.txt"),header=FALSE); #imports features.txt
activityType = read.table(file.path(data_URL, "activity_labels.txt"),header=FALSE); #imports activity_labels.txt

#Read the Activity files
activityTestData  <- read.table(file.path(data_URL, "test" , "Y_test.txt" ),header = FALSE)
activityTrainData <- read.table(file.path(data_URL, "train", "Y_train.txt"),header = FALSE)

#Read the Subject files
subjectTrainData <- read.table(file.path(data_URL, "train", "subject_train.txt"),header = FALSE)
subjectTestData  <- read.table(file.path(data_URL, "test" , "subject_test.txt"),header = FALSE)

#Read Fearures files
featuresTestData  <- read.table(file.path(data_URL, "test" , "X_test.txt" ),header = FALSE)
featuresTrainData <- read.table(file.path(data_URL, "train", "X_train.txt"),header = FALSE)

# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrainData)  = "subjectId";
colnames(featuresTrainData)        = features[,2]; 
colnames(activityTrainData)        = "activityId";

colnames(subjectTestData)  = "subjectId";
colnames(featuresTestData)        = features[,2]; 
colnames(activityTestData)        = "activityId";

# Create the final training set by merging activityTrainData, subjectTrainData, and featuresTrainData
trainingData = cbind(activityTrainData,subjectTrainData,featuresTrainData);

# Create the final test set by merging the activityTestData, subjectTestData and featuresTestData data
testData = cbind(activityTestData,subjectTestData,featuresTestData);

# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
fVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
finalData = finalData[fVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyDataSet    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyDataSet with activityType to include descriptive acitvity names
tidyDataSet    = merge(tidyDataSet,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyDataSet, './tidyData.txt',row.name=FALSE,sep='\t');