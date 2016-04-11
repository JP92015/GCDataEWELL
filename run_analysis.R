#run_analysis.R
# clear workspace
rm(list = ls())
#load libraries: tidyr,dplyr and data.table
library(tidyr)
library(dplyr)
library(data.table)
##
#read training samples
#read Y_train
Y_train<-extract_numeric(readLines("./UCI HAR Dataset/train/y_train.txt", n = -1))
#read subject_train rows
Subject_train<-extract_numeric(readLines("./UCI HAR Dataset/train/subject_train.txt", n = -1))
#read measurements training samples
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
#
#read test samples
#read Y_test
Y_test<-extract_numeric(readLines("./UCI HAR Dataset/test/y_test.txt", n = -1))
#read subject_train rows
Subject_test<-extract_numeric(readLines("./UCI HAR Dataset/test/subject_test.txt", n = -1))
#read measurements training samples
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
##
##
#Add Ids and activities
X_train_M<-mutate(X_train,subject=Subject_train,activity=Y_train)
X_test_M<-mutate(X_test,subject=Subject_test,activity=Y_test)
##
# join datasets
Unified_table<-rbind(X_train_M, X_test_M)
##
#Preparing selection of means and standard deviations
#read variable names
variables<-read.table("./UCI HAR Dataset/features.txt")
#grep means in feature vector
i2<-grep("mean()",variables$V2) 
#grep stds in feature vector
i3<-grep("std()",variables$V2) 
# reorder columns and
# extract only measurements of mean and standard deviation
Unified_table_red <-select(Unified_table, subject,activity,i2,i3)
#
#Generate variable names
#Select descriptive variable names
test1<-transpose(variables)
test2<-select(test1,i2,i3)
#
#Transpose for replacement of regular expressions in variable names by "_"
test3<-transpose(test2)
#Remove regular expressions from variable names.
test<-gsub("-", "", test3$V2, fixed = TRUE)
test<-gsub(",", "", test, fixed = TRUE)
test<-gsub("()", "", test, fixed = TRUE)
test<-gsub(")", "", test, fixed = TRUE)
test<-gsub("(", "", test, fixed = TRUE)
test<-tolower(test)
##
#replace variable names in X_train and X_test by descriptive names
names(Unified_table_red)[3:81]<-test
##
# Replace activity by names
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
rep_activity_labels=function(s){s=activity_labels[s,2]}
Unified_table_red<-mutate(Unified_table_red,activity=rep_activity_labels(activity))
#
#Define subject as factor
Unified_table_red<-mutate(Unified_table_red, subject=as.factor(subject))
###
#create new table
var2=as.name(test[1])
new_table<-summarise(group_by(Unified_table_red, subject ,activity), m = mean(var2))
names(new_table)[3]<-test[1]
for(n in c(2:79)) {
  var2=as.name(test[n])
  ZW2<-summarise(group_by(Unified_table_red, subject ,activity), m = mean(var2))
  names(ZW2)[3]<-test[n]
  new_table<-merge(new_table, ZW2,by = intersect(names(new_table), names(ZW2)))}
# Order subjects
new_table<-arrange(new_table, subject,activity)
# store new_table to file
write.table(new_table,file="New_Table.txt",row.name=FALSE)
