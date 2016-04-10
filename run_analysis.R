run_analysis <- function() {
  
  ## launch dplyr
  install.packages("dplyr")
  library(dplyr)
  
  
  # Step 1 - Merges the training and the test sets to create one data set.
  
  setwd("/UCI HAR Dataset")
  
  #### 1. read training data  
  
  ## subject_train: subject who performed the activity for each window sample. Its range is from 1 to 30.
  subject_train <- read.table("train/subject_train.txt", col.names = c("subjectId"))
  
  ## X_train: training set
  X_train <- read.table("train/X_train.txt", header = FALSE)
  
  ## y_train: training labels
  y_train <- read.table("train/y_train.txt", col.names=c("labelsId"))
  
  ### in order to improve merging efficiency we create a primary key for each table
  subject_train$primkey <- as.numeric(rownames(subject_train))
  X_train$primkey <- as.numeric(rownames(X_train))
  y_train$primkey <- as.numeric(rownames(y_train))
    
  #### 2. merge training data
  
  ## trainingSet: consolidate table of subject, training set and training labels
  trainingSet <- merge(subject_train,y_train,all = TRUE) %>% merge(X_train, all = TRUE)
  
  #### 3. read test data  
  
  ## subject_test: subject who performed the activity for each window sample. Its range is from 1 to 30.
  subject_test <- read.table("test/subject_test.txt", col.names = c("subjectId"))
  
  ## X_test: test set
  X_test <- read.table("test/X_test.txt", header = FALSE)
  
  ## y_test: test labels
  y_test <- read.table("test/y_test.txt", col.names=c("labelsId"))
  
  ### in order to improve merging efficiency we create a primary key for each table
  subject_test$primkey <- as.numeric(rownames(subject_test))
  X_test$primkey <- as.numeric(rownames(X_test))
  y_test$primkey <- as.numeric(rownames(y_test))
  
  #### 4. merge training data
  
  ## trainingSet: consolidate table of subject, training set and training labels
  testSet <- merge(subject_test,y_test,all = TRUE) %>% merge(X_test, all = TRUE)
  
  #### 5. bind training and test data
  dataSet <- rbind(trainingSet,testSet)
  
  # Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #### 1. import the features
  
  ## features: List of all features
  features <- read.table("features.txt", col.names = c("featureId", "featureName"))
  
  #### 2. select the list of features
  
  ## features_list: List of features including mean() or std()
  features_list <- features[grepl("mean\\(\\)", features$featureName) | grepl("std\\(\\)",features$featureName),]
  
  #### 3. extract desired data
  
  ## dataSet2: data set including the data of selected features 
  dataSet2 <- dataSet[,c(1:3,features_list$featureId+3)]
  
  # Step 3 - Uses descriptive activity names to name the activities in the data set

  #### 1. import the activity names
  ## activity_labels: Links the class labels with their activity name
  
  activity_labels <- read.table("activity_labels.txt", col.names=c("labelsId", "activityName")) 
  
  #### 2. merge activity_labels with dataset
  dataSet3 <- merge(dataSet2,activity_labels)  
  
  # Step 4 - Appropriately labels the data set with descriptive variable names.
  
  #### 1. import the activity names
  features_list$featureName <- gsub("\\(\\)", "", features_list$featureName)
  
  #### 2. create a new dataset (dataSet4) to change the name of the columns
  dataSet4 <- dataSet3
  
  #### 3. change names
  for (i in 1:length(features_list$featureName)) {
    colnames(dataSet4)[i + 3] <- features_list$featureName[i]
  }
  
  # Step 5 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  #### 1. exclude columns to simplify the 
  excludedColumns <- c("primkey","activityName")
  dataSet5 <- dataSet4[,!(names(dataSet4) %in% excludedColumns)]
  
  #### 2. compute the value we want by subject and nature of activity
  consoData <-aggregate(dataSet5, by=list(subject = dataSet5$subjectId, activity = dataSet5$labelsId), FUN=mean, na.rm=TRUE)
  
  #### 3. supress redundancy
  excludedColumns <- c("subject","activity")
  consoData <- consoData[,!(names(consoData) %in% excludedColumns)]
  
  #### 4. reintegrate activity labels
  consoData = merge(consoData, activity_labels)
  
  #### 5. export to txt file
  write.table(file="gandCDataCourseFinal.txt", x=consoData, row.names = FALSE)
  
  }