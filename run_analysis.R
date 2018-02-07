setwd("C:/Users/TEMP.Admin-PC/Google Drive/Data Science DOST/Getting and Cleaning Data/Programming Assignment")

library(plyr)
library(dplyr)
library(tidyr)

# UCI HAR Datasets------------
list.files("./UCI HAR Dataset/test")
list.files("./UCI HAR Dataset/train")
list.files("./UCI HAR Dataset")

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")


# 1. Merge training & test sets adding "Test"&"Train" id-------
x_test_ed <- x_test %>% tbl_df() %>% mutate(V562 = "Test")
x_train_ed <- x_train %>% tbl_df() %>% mutate(V562 = "Train")
merge_data_set <- rbind(x_test_ed, x_train_ed)


# 2. Extracts the mean and standard deviation for each measurement----
unique_names <- make.names(features$V2, unique=TRUE) %>% append("Test or Train")

colnames(merge_data_set) <- unique_names

mean_std_var <- grep("mean|std|test", unique_names, TRUE, value = TRUE)

merge_mean_std <- select(merge_data_set, mean_std_var)

merge_mean_std$`Test or Train` <- as.factor(merge_mean_std$`Test or Train`)

# 3. Addition of escriptive activity names----
activities <- rbind(y_test, y_train)
activities$V1 <- as.factor(activities$V1) %>% revalue(c("1" = "WALKING",
                                                        "2" = "WALKING_UPSTAIRS",
                                                        "3" = "WALKING_DOWNSTAIRS",
                                                        "4" = "SITTING",
                                                        "5" = "STANDING",
                                                        "6" = "LAYING"))
colnames(activities) <- c("Activity")
merge_activities <- cbind(activities, merge_mean_std)

# Add the subjects in the main data set
subjects_total <- rbind(subject_test, subject_train)
colnames(subjects_total) <- c("Subject")
final_merged <- cbind(subjects_total, merge_activities )

# Reorder the variables Subject > Activity > Test or Train
final_merged <- select(final_merged, "Subject", "Activity", "Test or Train", everything())

# 4. Appropriate labels of data set with descriptive variable names.
colnames <-colnames(final_merged)
colnames <- make.names(colnames, unique=TRUE)

# Remove/replace unnecessary characters
colnamesclean<-gsub("-", " ", colnames) # "-" -> space
colnamesclean<-gsub("\\.", " ", colnamesclean) # "." -> space
colnamesclean<-gsub("\\  ", " ", colnamesclean) # "." -> space
colnamesclean<-gsub("\\  ", " ", colnamesclean) # "." -> space
colnamesclean<-gsub("\\  ", " ", colnamesclean) # "." -> space
colnamesclean<-gsub("tBody", "Body", colnamesclean) # delete "t"
colnamesclean<-gsub("tGravity", "Gravity", colnamesclean) # delete "t"
colnamesclean<-gsub("fBody", "Body", colnamesclean) # delete "f"
colnamesclean<-gsub("BodyBody", "Body", colnamesclean) # delete double "Body"
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean) # delete start and end spaces

# Change colnames with cleaned colnames
colnames(final_merged) <- colnamesclean

str(final_merged)


# 5. independent tidy_data_set data set with the average of each 
# variable for each activity and each subject

tidy_data_set <- final_merged    

# Make colnames unique
colnames(tidy_data_set) <- make.names(colnames(tidy_data_set) , unique=TRUE)

# Convert df to tbl_df
tidy_data_set <- tbl_df(tidy_data_set)

# Group the data by subject and activity and Test.or.Train variables
grouped_data <-group_by(tidy_data_set, Subject, Activity, Test.or.Train)

# Calculation of mean based on grouped data
grouped_mean <- summarise_all(grouped_data, funs(mean))

# Attach again clean names from unique names
colnames(grouped_mean) <- colnamesclean

# Overview of tidy data set 
grouped_mean[1:10, 1:6]

# Tidy data set with "Test or Train" variable
write.table(grouped_mean, file="tidyDataSet.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)
