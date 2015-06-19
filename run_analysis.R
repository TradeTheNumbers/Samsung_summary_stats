run_analysis <- function(...){
  
## load necessary packages into library
library(data.table)
library(dplyr)

## Create directory, after checking that one doesn't already exist
if(!file.exists("./Samsung_Data")){dir.create("./Samsung_Data")}

## Assign url where file can be downloaded
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

##Download entire zipped file, storing in a temporary file
temp <- tempfile()
download.file(url1, temp)

##Store "test" data sets
test_data <- read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"))
test_subj <- read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"))
test_activity <- read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"))

##Store "train" data sets
train_data <- read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"))
train_subj <- read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"))
train_activity <- read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"))

##Unlink/release the "master" temporary file
unlink(temp)

##Merge the "test" and "train" data sets
full_data <- rbind(test_data,train_data)
  
##Subset data, keeping only columns/variables that include a "mean" or "std" measurement 
sub_data <- full_data[,c(1:6,41:46,81:86,121:126,161:166,201:202,214:215,227:228,240:241,253:254,266:271,345:350,424:429,503:504,516:517,529:530,542:543)]

##Insert "Activity Code" column, from y_test and y_train, and "Subject" column, from subject_test and subject_train, to the combined and subsetted data set
subject_col <- rbind(test_subj,train_subj)
activity_col <- rbind(test_activity,train_activity)


sub2_data <- cbind(subject_col, activity_col, sub_data)

##Label each column/variable with descriptive name

colnames(sub2_data) <- c("Subject",
                         "Activity",
                         "tBodyAcc-mean()-X",
                         "tBodyAcc-mean()-Y",
                         "tBodyAcc-mean()-Z",
                         "tBodyAcc-std()-X",
                         "tBodyAcc-std()-Y",
                         "tBodyAcc-std()-Z",
                         "tGravityAcc-mean()-X",
                         "tGravityAcc-mean()-Y",
                         "tGravityAcc-mean()-Z",
                         "tGravityAcc-std()-X",
                         "GravityAcc-std()-Y",
                         "tGravityAcc-std()-Z",
                         "tBodyAccJerk-mean()-X",
                         "tBodyAccJerk-mean()-Y",
                         "tBodyAccJerk-mean()-Z",
                         "tBodyAccJerk-std()-X",
                         "tBodyAccJerk-std()-Y",
                         "tBodyAccJerk-std()-Z",
                         "tBodyGyro-mean()-X",
                         "tBodyGyro-mean()-Y",
                         "tBodyGyro-mean()-Z",
                         "tBodyGyro-std()-X",
                         "tBodyGyro-std()-Y",
                         "tBodyGyro-std()-Z",
                         "tBodyGyroJerk-mean()-X",
                         "tBodyGyroJerk-mean()-Y",
                         "tBodyGyroJerk-mean()-Z",
                         "tBodyGyroJerk-std()-X",
                         "tBodyGyroJerk-std()-Y",
                         "tBodyGyroJerk-std()-Z",
                         "tBodyAccMag-mean()",
                         "tBodyAccMag-std()",
                         "tGravityAccMag-mean()",
                         "tGravityAccMag-std()",
                         "tBodyAccJerkMag-mean()",
                         "tBodyAccJerkMag-std()",
                         "tBodyGyroMag-mean()",
                         "tBodyGyroMag-std()",
                         "tBodyGyroJerkMag-mean()",
                         "tBodyGyroJerkMag-std()",
                         "fBodyAcc-mean()-X",
                         "fBodyAcc-mean()-Y",
                         "fBodyAcc-mean()-Z",
                         "fBodyAcc-std()-X",
                         "fBodyAcc-std()-Y",
                         "fBodyAcc-std()-Z",
                         "fBodyAccJerk-mean()-X",
                         "fBodyAccJerk-mean()-Y",
                         "fBodyAccJerk-mean()-Z",
                         "fBodyAccJerk-std()-X",
                         "fBodyAccJerk-std()-Y",
                         "fBodyAccJerk-std()-Z",
                         "fBodyGyro-mean()-X",
                         "fBodyGyro-mean()-Y",
                         "fBodyGyro-mean()-Z",
                         "fBodyGyro-std()-X",
                         "fBodyGyro-std()-Y",
                         "fBodyGyro-std()-Z",
                         "fBodyAccMag-mean()",
                         "fBodyAccMag-std()",
                         "fBodyBodyAccJerkMag-mean()",
                         "fBodyBodyAccJerkMag-std()",
                         "fBodyBodyGyroMag-mean()",
                         "fBodyBodyGyroMag-std()",
                         "fBodyBodyGyroJerkMag-mean()",
                         "fBodyBodyGyroJerkMag-std()"                      
                         )

##Convert numerical activity codes (1-6) into character descriptions (i.e. "WALKING"..)

sub2_data[sub2_data$Activity==1,2] <- "WALKING"
sub2_data[sub2_data$Activity==2,2] <- "WALKING_UPSTAIRS"
sub2_data[sub2_data$Activity==3,2] <- "WALKING_DOWNSTAIRS"
sub2_data[sub2_data$Activity==4,2] <- "SITTING"
sub2_data[sub2_data$Activity==5,2] <- "STANDING"
sub2_data[sub2_data$Activity==6,2] <- "LAYING"

## Convert data.frame to data.table for further processing
dt <- tbl_df(sub2_data)

## Calc the mean of each variable, by subject and by activity
dt_summary <- dt %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

##Convert data.table back to data.frame
dt_summary <- as.data.frame(dt_summary)

## Write out data.frame to txt file
write.table(dt_summary, file = "./Samsung_summary_stats.txt", row.name=FALSE)

}