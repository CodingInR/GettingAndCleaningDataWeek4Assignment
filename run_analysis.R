# CodingInR
#  
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#qdap for mgsub vs long list of gsub statements
#install.packages("qdap")
#library(qdap)

#dplyr for final tidy data set
install.packages("dplyr")
library(dplyr)

#Dataset URL
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

#Download dataset if it hasn't been already
if(!file.exists("data.zip")){download.file(URL, destfile = "data.zip")}

#Unzip dataset if it hasn't been already
if(!dir.exists("UCI HAR Dataset/")){unzip(zipfile = "data.zip")}

#Load Data
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
feat <- read.table("UCI HAR Dataset/features.txt")
act <- read.table("UCI HAR Dataset/activity_labels.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")

#combine the training, test, activity, features and subject data (concatnated by rows), step 1
feat_comb <- rbind(X_train, X_test)
act_comb <- rbind(Y_train, Y_test)
sub_comb <- rbind(sub_train, sub_test)

#combine data step 2
all_data <- cbind(sub_comb, act_comb, feat_comb)

#provide column names for combined data set 
Names <- c("subject", "activity", as.character(feat$V2))
names(all_data) <- Names
#factorize activity (from numbers to lables)
all_data$activity <- factor(all_data$activity, labels = act[,2])

#selected/reduced dataset (assumes "meanFreq()" is also a mean - assignment not clear)
sel_vars<- grepl("subject|activity|mean|std", Names)
reduced_data <- all_data[,sel_vars == TRUE]

#clean up environment (commented out as optional)
#rm(list=setdiff(ls(), "reduced_data"))

#tidy up variable names per the descriptions in features_info.txt
ugly_names <- c("^t","^f","Acc","Mag","Gyro","BodyBody","std")
tidy_names <- c("Time","Frequency","Acceleration","Magnitude","Gyrometer","Body","Standard_Deviation")
names(reduced_data) <- mgsub(ugly_names,tidy_names,names(reduced_data),fixed = FALSE)

#create final tidy data set
tidy_data <- reduced_data %>% group_by(activity, subject) %>% summarize_each(funs(mean))
write.table(tidy_data,"tidy_data.txt", row.name=FALSE, sep='\t')
