################################################################################

## Coursera Getting and Cleaning Data Course Project
## André Valente do Couto
## 23.04.2015

# runAnalysis.r      

################################################################################


# 0.
print("# 0. Obtain data from web datasource")
if(!file.exists("./data")) 
{  
    dir.create("./data")  
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/
 getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./data/Dataset.zip"))
{
    download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
    unzip(zipfile="./data/Dataset.zip",exdir="./data")              # unzip file
}
dir_base <- file.path("./data" , "UCI HAR Dataset")
#files <- list.files(dir_base, recursive=TRUE)


# 1.
print("# 1. Merges the training and the test sets to create one data set.")

# 1.1. read feature train and test and combine your rows in one datatable
tmp1 <- read.table(file.path(dir_base, "train" , "X_train.txt"), header = FALSE)
tmp2 <- read.table(file.path(dir_base, "test"  , "X_test.txt" ), header = FALSE)
dF <- rbind(tmp1, tmp2)         # dataFeature

# 1.2. read subject train and test and combine your rows in one datatable
tmp1 <- read.table(file.path(
    dir_base, "train" , "subject_train.txt" ), header = FALSE)
tmp2 <- read.table(file.path(
    dir_base, "test"  , "subject_test.txt"  ), header = FALSE)
dS <- rbind(tmp1, tmp2)         # dataSubject

# 1.3. read Activity train and test and combine your rows in one datatable
tmp1 <- read.table(file.path(dir_base, "train" , "y_train.txt"), header = FALSE)
tmp2 <- read.table(file.path(dir_base, "test"  , "y_test.txt" ), header = FALSE)
dA <- rbind(tmp1, tmp2)         #dataActivity


# 2.
print(paste("# 2. Extracts only the measurements on the mean and",
      "standard deviation for each measurement."))

# read features file for the names in dF
features <- read.table(file.path(dir_base, "features.txt"), header = FALSE)
# apply names for dF by second columns of features
names(dF) <- features[, 2]
# subset dF by index on features with contains -mean() and -std() in 2 column
dF <- dF[, grep("-(mean|std)\\(\\)", features[, 2])]
# remove () from names of dF
names(dF) <- gsub("\\(|\\)", "", names(dF))
# lower case for all names of dF
names(dF) <- tolower(names(dF))


# 3.
print(paste("# 3. Uses descriptive activity names to name the",
            "activities in the data set."))
# read de activity_labels.txt file
activities <- read.table(file.path(dir_base, "activity_labels.txt"),
                         header = FALSE)
# remove de underline and low case on descriptions of activities
activities[, 2] <- gsub("_", "", tolower(as.character(activities[, 2])))
# link description in activities[,2], usind dA[,1] as index for 2 description 
dA[,1] = activities[dA[,1], 2]
# assign name for dA
names(dA) <- "activity"


# 4.
print("# 4. Appropriately labels the data set with descriptive activity names.")
# assign name for dS
names(dS) <- "subject"
# merge all dataFrames in one named all_data
all_data <- cbind(dS, dA, dF)
# write all_data in file
write.table(all_data, "merge_clean_data.txt")

# 5. 
print(paste("# 5. Creates a 2nd, independent tidy data set with the average of",
            " each variable for each activity and each subject."))
# calculate mean by subject and activity groups using ddply function
mean_data <- 
    ddply(all_data, .(subject, activity), function(x) colMeans(x[, 3:66]))
# order by column subject and activity
mean_data<-mean_data[order(mean_data$subject,mean_data$activity),]
# write tiny file with average by groups
write.table(mean_data, "average_data.txt", row.name=FALSE)
