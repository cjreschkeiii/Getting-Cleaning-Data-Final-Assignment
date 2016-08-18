# Instructions:
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis.
##########################################################################################################

# Review criteria:
# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# The README that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.
##########################################################################################################

# This script will perform the following: 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
##########################################################################################################

# 1. Merges the training and the test sets to create one data set.
# Set working directory to the location of UCI HAR Dataset
setwd("F:/Trainings/John Hopkins Data Science Specialization/Course 3 - Getting and Cleaning Data/UCI HAR Dataset")

# Read in the data from files
features <- read.table('./features.txt',header=FALSE);
activity_labels <- read.table('./activity_labels.txt',header=FALSE);
subject_train <- read.table('./train/subject_train.txt',header=FALSE);
x_train <- read.table('./train/x_train.txt',header=FALSE);
y_train <- read.table('./train/y_train.txt',header=FALSE);
subject_test <- read.table('./test/subject_test.txt',header=FALSE);
x_test <- read.table('./test/x_test.txt',header=FALSE); 
y_test <- read.table('./test/y_test.txt',header=FALSE);

# Assign column names to the data imported above
colnames(activity_labels) <- c('activityId','activityType');
colnames(subject_train) <- "subjectId";
colnames(x_train) <- features[,2]; 
colnames(y_train) <- "activityId";
colnames(subject_test) <- "subjectId";
colnames(x_test) <- features[,2]; 
colnames(y_test) <- "activityId";

# Create the final training and test data sets
training_data <- cbind(y_train,subject_train,x_train);
test_data <- cbind(y_test,subject_test,x_test);

# Combine training and test data to create a final data set
final_data <- rbind(training_data,test_data);

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# Create a vector for the column names from the final_data, which will be used to select the desired mean() & stddev() columns
colNames <- colnames(final_data); 

# Create vector for defining ID, mean and standard deviation
mean_and_stddev <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
    
# Subset based on the mean_and_stddev to keep only desired columns
final_data_MeanAndStdDev <- final_data[mean_and_stddev == TRUE];

# 3. Uses descriptive activity names to name the activities in the data set
final_data_ActivityNames <- merge(final_data_MeanAndStdDev,activity_labels,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames <- colnames(final_data_ActivityNames);

# 4. Appropriately labels the data set with descriptive variable names
# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
    colNames[i] <- gsub("\\()","",colNames[i])
    colNames[i] <- gsub("-std$","StdDev",colNames[i])
    colNames[i] <- gsub("-mean","Mean",colNames[i])
    colNames[i] <- gsub("^(t)","time",colNames[i])
    colNames[i] <- gsub("^(f)","freq",colNames[i])
    colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassign the new descriptive column names to the data set
colnames(final_data_ActivityNames) = colNames;

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Making second tidy data set
tidy_data <- final_data_ActivityNames[,names(final_data_ActivityNames) != 'activityType'];
tidy_data <- aggregate(tidy_data[,names(tidy_data) != c('activityId','subjectId')],by=list(activityId=tidy_data$activityId,subjectId = tidy_data$subjectId),mean);
tidy_data <- merge(tidy_data,activity_labels,by='activityId',all.x=TRUE);

# Export the tidy_data set 
write.table(tidy_data, './tidy_data.txt',row.names=TRUE,sep='\t')
