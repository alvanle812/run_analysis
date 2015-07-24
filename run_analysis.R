## Instruction
##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names. 
##5. From the data set in step 4, creates a second, independent tidy data set with the average
##   of each variable for each activity and each subject.

## 0. Loading the raw data
getwd()
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/dataset.zip", method = "curl")
unzip("./data/dataset.zip", exdir = "./data")


# 1. Combine as one data set
library(dplyr)
trainx <- read.table("./data/UCI HAR Dataset/train/X_train.txt")

trainy <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
trainy <- rename(trainy, label = V1)



trainsub <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
trainsub <- rename(trainsub, subject = V1)

train <- cbind(trainy, trainsub)
train <- cbind(trainx, train)

testx <- read.table("./data/UCI HAR Dataset/test/X_test.txt")

testy <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
testy <- rename(testy, label = V1)

testsub <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
testsub <- rename(testsub, subject = V1)

test <- cbind(testy, testsub)
test <- cbind(testx, test)


data <- rbind(test, train)

# 2. Extract mean and std
data_msd <- cbind(data[, 1:6], data[, 41:46], data[, 81:86], data[, 121:126], data[, 161:166])
data_msd <- cbind(data_msd, data[, 201:202], data[, 214:215], data[, 227:228], data[, 240:241], data[, 253:254])
data_msd <- cbind(data_msd, data[, 266:271], data[, 345:350], data[, 424:429])
data_msd <- cbind(data_msd, data[, 503:504], data[, 516:517], data[, 529:530], data[, 542:543])
data_msd <- cbind(data$subject, data$label, data_msd)


data_msd <- rename(data_msd, subject = data_msd[, 1], label = data_msd[, 2])
colnames(data_msd)[1] <- "subject"
colnames(data_msd)[2] <- "label"

# 3. Renaming activity
attach(data_msd)
data_msd$activity[label == 1] <- "WALKING"
data_msd$activity[label == 2] <- "WALKING_UPSTAIRS"
data_msd$activity[label == 3] <- "WALKING_DOWNSTAIRS"
data_msd$activity[label == 4] <- "SITTING"
data_msd$activity[label == 5] <- "STANDING"
data_msd$activity[label == 6] <- "LAYING"
detach(data_msd)


# 4. Labels the data set with descriptive variable names
## data_msd <- rename(data_msd, tBodyAcc_mean_X = V1, tBodyAcc_mean_Y = V2, tBodyAcc_mean_Z =V3,
##                   tBodyAcc_std_X = V4, tBodyAcc_std_Y = V5, tBodyAcc_std_Z = V6)

colnames(data_msd) <- c("subject", "label", "tBodyAcc_mean_X", "tBodyAcc_mean_Y", "tBodyAcc_mean_Z", "tBodyAcc_std_X", "tBodyAcc_std_Y", "tBodyAcc_std_Z", 
                        "tGravityAcc_mean_X", "tGravityAcc_mean_Y", "tGravityAcc_mean_Z", "tGravityAcc_std_X", "tGravityAcc_std_Y", "tGravityAcc_std_Z",
                        "tBodyAccJerk_mean_X", "tBodyAccJerk_mean_Y", "tBodyAccJerk_mean_Z", "tBodyAccJerk_std_X", "tBodyAccJerk_std_Y", "tBodyAccJerk_std_Z",
                        "tBodyGyro_mean_X", "tBodyGyro_mean_Y", "tBodyGyro_mean_Z", "tBodyGyro_std_X", "tBodyGyro_std_Y", "tBodyGyro_std_Z",
                        "tBodyGyroJerk_mean_X", "tBodyGyroJerk_mean_Y", "tBodyGyroJerk_mean_Z", "tBodyGyroJerk_std_X", "tBodyGyroJerk_std_Y", "tBodyGyroJerk_std_Z",
                        "tBodyAccMag_mean", "tBodyAccMag_std",
                        "tGravityAccMag_mean", "tGravityAccMag_std",
                        "tBodyAccJerkMag_mean", "tBodyAccJerkMag_std",
                        "tBodyGyroMag_mean", "tBodyGyroMag_std",
                        "tBodyGyroJerkMag_mean", "tBodyGyroJerkMag_std",
                        "fBodyAcc_mean_X", "fBodyAcc_mean_Y", "fBodyAcc_mean_Z", "fBodyAcc_std_X", "fBodyAcc_std_Y", "fBodyAcc_std_Z",
                        "fBodyAccJerk_mean_X", "fBodyAccJerk_mean_Y", "fBodyAccJerk_mean_Z", "fBodyAccJerk_std_X", "fBodyAccJerk_std_Y", "fBodyAccJerk_std_Z",
                        "fBodyGyro_mean_X", "fBodyGyro_mean_Y", "fBodyGyro_mean_Z", "fBodyGyro_std_X", "fBodyGyro_std_Y", "fBodyGyro_std_Z",
                        "fBodyAccMag_mean", "fBodyAccMag_std",
                        "fBodyBodyAccJerkMag_mean", "fBodyBodyAccJerkMag_std",
                        "fBodyBodyGyroMag_mean", "fBodyBodyGyroMag_std",
                        "fBodyBodyGyroJerkMag_mean", "fBodyBodyGyroJerkMag_std",
                        "activity")

# 5. creates a second, independent tidy data set with the average of each variable for each activity and each subject.
data_small <- aggregate(data_msd, list(Subject = data_msd$subject, Activity = data_msd$activity), mean)
data_small <- data_small[, -3]
data_small <- data_small[, -70]
write.table(data_small, file = "tidydata.txt", sep = ",", row.name=FALSE)
