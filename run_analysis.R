## Input files: X_test.txt, X_train.txt, Y_test.txt, Y_train.txt, subject_test.txt, subject_train.txt
library(reshape2)

run_analysis <- function() {
  #Read and merge X data sets
  X_test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
  X_train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
  X_join_data <- rbind(X_test_data, X_train_data)
  
  #Check X tables dimensions
  dim(X_test_data) # (2947, 561)
  dim(X_train_data) # (7352, 561)
  dim(X_join_data) # (10299, 561)
  
  #Read and merge Y data sets
  Y_test_data <- read.table("./UCI HAR Dataset/test/Y_test.txt")
  Y_train_data <- read.table("./UCI HAR Dataset/train/Y_train.txt")
  Y_join_data <- rbind(Y_test_data, Y_train_data)
  
  # Check Y tables dimensions
  dim(Y_test_data) # (2947, 1)
  dim(Y_train_data) # (7352, 1)
  dim(Y_join_data) # (10299, 1)
  
  #Read and merge subject data sets
  subject_test_data <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  subject_train_data <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  subject_join_data <- rbind(subject_test_data, subject_train_data)
  
  # Check subject tables dimensions
  dim(subject_test_data) # (2947, 1)
  dim(subject_train_data) # (7352, 1)
  dim(subject_join_data) # (10299, 1)
  
  ## Extract the mean and STDev for each measurement
  features <- read.table("./UCI HAR Dataset/features.txt")
  meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  
  X_join_datanew <- X_join_data[, meanstdindex]
  colnames(X_join_datanew) <- features[meanstdindex, 2] 
  
  colnames(X_join_datanew) <- gsub("\\(|\\)", "", colnames(X_join_datanew)) 
  colnames(X_join_datanew) <- gsub("-", ".", colnames(X_join_datanew))
  colnames(X_join_datanew) <- tolower(colnames(X_join_datanew))
  
  ## Name the activities in the data set.
  activity <- read.table("./UCI HAR Dataset/activity_labels.txt")
  
  activity[, 2] <- tolower(gsub("_","", activity[, 2])) # Remove invalid characters
  
  activity_label <- activity[Y_join_data[, 1], 2]
  Y_join_data[, 1] <- activity_label
  
  colnames(Y_join_data) <- "activity"
  
  ## Label data sets with descriptive activity names
  colnames(subject_join_data) <- "subject"
  
  cleandata <- cbind(subject_join_data, Y_join_data, X_join_datanew)
  
  write.table(cleandata, "cleandata.txt")
  
  ## Step 5
  meltdf <- melt(cleandata, id=c("activity", "subject"))
  
  tidydf <- dcast(meltdf, activity + subject ~ variable, mean)
  write.table(tidydf, "tidy_average_data.txt", row.names = F, col.names= T, sep = ",")
}