# REMOVING PREVIOUS VARIABLES

rm(list=ls())

# LOADING LIBRARIES

library(data.table)
library(dplyr)

# GETTING DATA

features_path <- file.path("UCI HAR Dataset","features.txt")
train_path <- file.path("UCI HAR Dataset","train","X_train.txt")
test_path <- file.path("UCI HAR Dataset","test","X_test.txt")
activity_labels_path <- file.path("UCI HAR Dataset","activity_labels.txt")
train_label_path <- file.path("UCI HAR Dataset","train","y_train.txt")
test_label_path <- file.path("UCI HAR Dataset","test","y_test.txt")
train_subject_path <- file.path("UCI HAR Dataset","train","subject_train.txt")
test_subject_path <- file.path("UCI HAR Dataset","test","subject_test.txt")

features <- read.table(features_path,sep=" ")
activity_labels <- read.table(activity_labels_path)
train_data <- read.table(train_path)
test_data <- read.table(test_path)
train_label <- read.table(train_label_path)
test_label <- read.table(test_label_path)
train_subject <- read.table(train_subject_path)
test_subject <- read.table(test_subject_path)

# 1. Merges the training and the test sets to create one data set

dataset <- rbind(train_data,test_data)
colNames <- features[[2]]
names(dataset) <- colNames


# 2. Extracts only the measurements on the mean and standard deviation for each measurement

mean_and_std_index <- grep("(mean)|(std)",colNames)
dataset <- dataset[mean_and_std_index]

# 3. Uses descriptive activity names to name the activities in the data set

activity_id <- rbind(train_label,test_label)
activity_names <- merge(activity_id,activity_labels,by.x="V1",by.y="V1",sort=F)
subject <- rbind(train_subject,test_subject)

dataset$subject = subject
dataset$activityName = activity_names$V2

# 4. Appropriately labels the data set with descriptive variable names
colNames <- names(dataset)
colNames <- gsub("^t","time",colNames)
colNames <- gsub("^f","frequency",colNames)
colNames <- gsub("[-|(|)]","",colNames)
colNames <- sub("Acc","Accelerometer",colNames)
colNames <- sub("Gryo","Gyroscope",colNames)
colNames <- sub("Mag","Magnitude",colNames)
colNames <- sub("BodyBody","Body",colNames)
names(dataset)<-colNames

# 5. tidy data set with the average of each variable for each activity and each subject

group_by_activityName_and_subject <- group_by(dataset,activityName,subject)
average_tidy_dataset <- summarize_all(group_by_activityName_and_subject,"mean")
write.table(average_tidy_dataset,"tidy_data.txt",row.names=F)

# Creating CodeBook

code_book_df<-data.frame(rbind(summary(average_tidy_dataset)))
name_df<-data.frame(names(average_tidy_dataset))
names(code_book_df)<-names(average_tidy_dataset)
i=0
for(col_id in seq(1,ncol(code_book_df))){
	i=i+1
	write.table(name_df[col_id,1],"CODEBOOK.txt",quote=F,append=T,col.names=F,row.names=paste(as.character(i)," : "))
	write.table(code_book_df[,name_df[col_id,1]],"CODEBOOK.txt",quote=F,append=T,na="",col.names=F,row.names=rep("      ",6))
	
}
