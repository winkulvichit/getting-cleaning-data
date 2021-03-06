library(tidyr)
library(dplyr)

# Import dataset
filename <- "Coursera.zip"

# Checking if archive already exists.
if (!file.exists(filename)){
      fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
      unzip(filename) 
}

setwd("UCI HAR Dataset")

features <- read.table("features.txt")
activity <- read.table("activity_labels.txt")

train_x <- read.table("train/X_train.txt")
train_y <- read.table("train/y_train.txt")
train_subject <- read.table("train/subject_train.txt")
train <- cbind(train_subject, train_x, train_y)

test_x <- read.table("test/X_test.txt")
test_y <- read.table("test/y_test.txt")
test_subject <- read.table("test/subject_test.txt")
test <- cbind(test_subject, test_x, test_y)

# Merges the training and the test sets to create one data set.
df <- rbind(train, test)
names(df)[1:562] <- paste0("V", c("1":"562"))
names(df)[563] <- "activity"

# Extracts only the measurements on the mean and standard deviation for each measurement.
mean <- apply(df[2:562],2,mean,na.rm=TRUE)
sd <- apply(df[2:562],2,sd,na.rm=TRUE)
names(mean)[2:562] <- features[,2]
names(sd)[2:562] <- features[,2]

# Uses descriptive activity names to name the activities in the data set
df$activity = factor(df$activity, levels = c(1:6), labels = c(activity[,2]))

# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

means <- df %>% group_by(V1, activity) %>% select(-c(1,563)) %>% summarize_all(funs(mean))
names(means)[2:562] <- features[,2]
write.table(means, file = "final_table.txt", row.names = FALSE) 
