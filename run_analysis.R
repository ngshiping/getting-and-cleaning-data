### 1. Merges the training and the test sets to create one data set.

# Read training data
train.x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Read testing data
test.x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Merge training data
trainData <- cbind(train.subject, train.y, train.x)
rm(train.x, train.y, train.subject)

# Merge testing data
testData <- cbind(test.subject, test.y, test.x)
rm(test.x, test.y, test.subject)

# Merge training and testing data
fullData <- rbind(trainData, testData)
rm(trainData, testData)

### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Load feature names
featureName <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

# Extract mean and standard deviation for each measurement
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
finalData <- fullData[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])
rm(fullData, featureName, featureIndex)

### 3. Uses descriptive activity names to name the activities in the data set

# Load activity data
activityName <- read.table("./UCI HAR Dataset/activity_labels.txt")

# Replace 1 to 6 with activity names
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])
rm(activityName)

### 4. Appropriately labels the data set with descriptive variable names.

names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequency", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

write.table(finalData, "./finalTidyData.txt", row.names = FALSE)

### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
groupData <- finalData %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

write.table(groupData, "./finalMeanData.txt", row.names = FALSE)
