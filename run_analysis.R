## This script analyses the attached data as per instructions for Course Project
## for Getting and Cleaning Data course on Coursera.

## Should be run as summarize_data(clean_data)

## Most of the analyses will be done with dplyr package.
library(dplyr)

## Create function clean_data which returns a data set containing all std and
## mean variables grouped by subjects and activities.

clean_data <- function() {

	## Proper variable names
	proper_var_names <- c("SUB_ID",
						"ACT_ID",
						"ACT_NAME",
						"TBODYACC_MEAN_X",
						"TBODYACC_MEAN_Y",
						"TBODYACC_MEAN_Z",
						"TGRAVITYACC_MEAN_X",
						"TGRAVITYACC_MEAN_Y",
						"TGRAVITYACC_MEAN_Z",
						"TBODYACCJERK_MEAN_X",
						"TBODYACCJERK_MEAN_Y",
						"TBODYACCJERK_MEAN_Z",
						"TBODYGYRO_MEAN_X",
						"TBODYGYRO_MEAN_Y",
						"TBODYGYRO_MEAN_Z",
						"TBODYGYROJERK_MEAN_X",
						"TBODYGYROJERK_MEAN_Y",
						"TBODYGYROJERK_MEAN_Z",
						"TBODYACCMAG_MEAN",
						"TGRAVITYACCMAG_MEAN",
						"TBODYACCJERKMAG_MEAN",
						"TBODYGYROMAG_MEAN",
						"TBODYGYROJERKMAG_MEAN",
						"FBODYACC_MEAN_X",
						"FBODYACC_MEAN_Y",
						"FBODYACC_MEAN_Z",
						"FBODYACC_MEANFREQ_X",
						"FBODYACC_MEANFREQ_Y",
						"FBODYACC_MEANFREQ_Z",
						"FBODYACCJERK_MEAN_X",
						"FBODYACCJERK_MEAN_Y",
						"FBODYACCJERK_MEAN_Z",
						"FBODYACCJERK_MEANFREQ_X",
						"FBODYACCJERK_MEANFREQ_Y",
						"FBODYACCJERK_MEANFREQ_Z",
						"FBODYGYRO_MEAN_X",
						"FBODYGYRO_MEAN_Y",
						"FBODYGYRO_MEAN_Z",
						"FBODYGYRO_MEANFREQ_X",
						"FBODYGYRO_MEANFREQ_Y",
						"FBODYGYRO_MEANFREQ_Z",
						"FBODYACCMAG_MEAN",
						"FBODYACCMAG_MEANFREQ",
						"FBODYBODYACCJERKMAG_MEAN",
						"FBODYBODYACCJERKMAG_MEANFREQ",
						"FBODYBODYGYROMAG_MEAN",
						"FBODYBODYGYROMAG_MEANFREQ",
						"FBODYBODYGYROJERKMAG_MEAN",
						"FBODYBODYGYROJERKMAG_MEANFREQ",
						"ANGLE_TBODYACCMEAN_GRAVITY",
						"ANGLE_TBODYACCJERKMEAN_GRAVITYMEAN",
						"ANGLE_TBODYGYROMEAN_GRAVITYMEAN",
						"ANGLE_TBODYGYROJERKMEAN_GRAVITYMEAN",
						"ANGLE_X_GRAVITYMEAN",
						"ANGLE_Y_GRAVITYMEAN",
						"ANGLE_Z_GRAVITYMEAN",
						"TBODYACC_STD_X",
						"TBODYACC_STD_Y",
						"TBODYACC_STD_Z",
						"TGRAVITYACC_STD_X",
						"TGRAVITYACC_STD_Y",
						"TGRAVITYACC_STD_Z",
						"TBODYACCJERK_STD_X",
						"TBODYACCJERK_STD_Y",
						"TBODYACCJERK_STD_Z",
						"TBODYGYRO_STD_X",
						"TBODYGYRO_STD_Y",
						"TBODYGYRO_STD_Z",
						"TBODYGYROJERK_STD_X",
						"TBODYGYROJERK_STD_Y",
						"TBODYGYROJERK_STD_Z",
						"TBODYACCMAG_STD",
						"TGRAVITYACCMAG_STD",
						"TBODYACCJERKMAG_STD",
						"TBODYGYROMAG_STD",
						"TBODYGYROJERKMAG_STD",
						"FBODYACC_STD_X",
						"FBODYACC_STD_Y",
						"FBODYACC_STD_Z",
						"FBODYACCJERK_STD_X",
						"FBODYACCJERK_STD_Y",
						"FBODYACCJERK_STD_Z",
						"FBODYGYRO_STD_X",
						"FBODYGYRO_STD_Y",
						"FBODYGYRO_STD_Z",
						"FBODYACCMAG_STD",
						"FBODYBODYACCJERKMAG_STD",
						"FBODYBODYGYROMAG_STD",
						"FBODYBODYGYROJERKMAG_STD")

	## Load test and train data into seperate data frames and append them.
	test_data <- read.table("test/X_test.txt")
	test_data <- tbl_df(test_data)
	train_data <- read.table("train/X_train.txt")
	train_data <- tbl_df(train_data)
	mydata <- rbind(test_data, train_data)

	## Remove test and train data frames.
	rm(train_data, test_data)

	## Load varible names from features.txt, extract names and save them in
	## var_names vector. Create unique_var_names vector to clear duplicate names.
	## Remove features table as it is not needed any more.
	features <- read.table("features.txt")
	var_names <- as.character(features[,2])
	unique_var_names <- unique(var_names)
	rm(features)

	## Remove duplicate columns from mydata.
	mydata <- select(mydata, -which(duplicated(var_names)))

	## Apply unique_var_names to variable names in mydata, and remove var_names
	## and unique_var_names.
	names(mydata) <- unique_var_names
	rm(var_names, unique_var_names)

	## Select only columns which contain "mean" or "std" in column name.
	## mydata <- select(mydata, grep("mean\\(\\)|std\\(\\)", names(mydata)))
	mydata <- select(mydata, contains("mean"), contains("std")) ## cleaner

	## Load test and train subjects and activites and append them to mydata.
	## Properly name mydata variables.
	test_subjects <- read.table("test/subject_test.txt")
	train_subjects <- read.table("train/subject_train.txt")
	subjects <- rbind(test_subjects, train_subjects)
	names(subjects) <- c("SUB_ID")

	activity_names <- read.table("activity_labels.txt")
	test_activities <- read.table("test/y_test.txt")
	train_activities <- read.table("train/y_train.txt")
	activities <- rbind(test_activities, train_activities)
	activities <- inner_join(activities, activity_names, by = "V1")
	names(activities) <- c("ACT_ID", "ACT_NAME")

	mydata <- cbind(subjects, activities, mydata)
	mydata <- tbl_df(mydata)
	names(mydata) <- proper_var_names

	## Group data by subject id and activity id.
	mydata <- group_by(mydata, SUB_ID, ACT_ID)
	mydata
}

## Create a function summarize_data which takes input generated by clean_data
## function and returns a summarized data set with means of all accelerometer
## variables per subject per activity.
summarize_data <- function(mydata) {
	summary_data <- summarize(mydata,
									ACT_NAME[1],
									mean(TBODYACC_MEAN_X),
									mean(TBODYACC_MEAN_Y),
									mean(TBODYACC_MEAN_Z),
									mean(TGRAVITYACC_MEAN_X),
									mean(TGRAVITYACC_MEAN_Y),
									mean(TGRAVITYACC_MEAN_Z),
									mean(TBODYACCJERK_MEAN_X),
									mean(TBODYACCJERK_MEAN_Y),
									mean(TBODYACCJERK_MEAN_Z),
									mean(TBODYGYRO_MEAN_X),
									mean(TBODYGYRO_MEAN_Y),
									mean(TBODYGYRO_MEAN_Z),
									mean(TBODYGYROJERK_MEAN_X),
									mean(TBODYGYROJERK_MEAN_Y),
									mean(TBODYGYROJERK_MEAN_Z),
									mean(TBODYACCMAG_MEAN),
									mean(TGRAVITYACCMAG_MEAN),
									mean(TBODYACCJERKMAG_MEAN),
									mean(TBODYGYROMAG_MEAN),
									mean(TBODYGYROJERKMAG_MEAN),
									mean(FBODYACC_MEAN_X),
									mean(FBODYACC_MEAN_Y),
									mean(FBODYACC_MEAN_Z),
									mean(FBODYACC_MEANFREQ_X),
									mean(FBODYACC_MEANFREQ_Y),
									mean(FBODYACC_MEANFREQ_Z),
									mean(FBODYACCJERK_MEAN_X),
									mean(FBODYACCJERK_MEAN_Y),
									mean(FBODYACCJERK_MEAN_Z),
									mean(FBODYACCJERK_MEANFREQ_X),
									mean(FBODYACCJERK_MEANFREQ_Y),
									mean(FBODYACCJERK_MEANFREQ_Z),
									mean(FBODYGYRO_MEAN_X),
									mean(FBODYGYRO_MEAN_Y),
									mean(FBODYGYRO_MEAN_Z),
									mean(FBODYGYRO_MEANFREQ_X),
									mean(FBODYGYRO_MEANFREQ_Y),
									mean(FBODYGYRO_MEANFREQ_Z),
									mean(FBODYACCMAG_MEAN),
									mean(FBODYACCMAG_MEANFREQ),
									mean(FBODYBODYACCJERKMAG_MEAN),
									mean(FBODYBODYACCJERKMAG_MEANFREQ),
									mean(FBODYBODYGYROMAG_MEAN),
									mean(FBODYBODYGYROMAG_MEANFREQ),
									mean(FBODYBODYGYROJERKMAG_MEAN),
									mean(FBODYBODYGYROJERKMAG_MEANFREQ),
									mean(ANGLE_TBODYACCMEAN_GRAVITY),
									mean(ANGLE_TBODYACCJERKMEAN_GRAVITYMEAN),
									mean(ANGLE_TBODYGYROMEAN_GRAVITYMEAN),
									mean(ANGLE_TBODYGYROJERKMEAN_GRAVITYMEAN),
									mean(ANGLE_X_GRAVITYMEAN),
									mean(ANGLE_Y_GRAVITYMEAN),
									mean(ANGLE_Z_GRAVITYMEAN),
									mean(TBODYACC_STD_X),
									mean(TBODYACC_STD_Y),
									mean(TBODYACC_STD_Z),
									mean(TGRAVITYACC_STD_X),
									mean(TGRAVITYACC_STD_Y),
									mean(TGRAVITYACC_STD_Z),
									mean(TBODYACCJERK_STD_X),
									mean(TBODYACCJERK_STD_Y),
									mean(TBODYACCJERK_STD_Z),
									mean(TBODYGYRO_STD_X),
									mean(TBODYGYRO_STD_Y),
									mean(TBODYGYRO_STD_Z),
									mean(TBODYGYROJERK_STD_X),
									mean(TBODYGYROJERK_STD_Y),
									mean(TBODYGYROJERK_STD_Z),
									mean(TBODYACCMAG_STD),
									mean(TGRAVITYACCMAG_STD),
									mean(TBODYACCJERKMAG_STD),
									mean(TBODYGYROMAG_STD),
									mean(TBODYGYROJERKMAG_STD),
									mean(FBODYACC_STD_X),
									mean(FBODYACC_STD_Y),
									mean(FBODYACC_STD_Z),
									mean(FBODYACCJERK_STD_X),
									mean(FBODYACCJERK_STD_Y),
									mean(FBODYACCJERK_STD_Z),
									mean(FBODYGYRO_STD_X),
									mean(FBODYGYRO_STD_Y),
									mean(FBODYGYRO_STD_Z),
									mean(FBODYACCMAG_STD),
									mean(FBODYBODYACCJERKMAG_STD),
									mean(FBODYBODYGYROMAG_STD),
									mean(FBODYBODYGYROJERKMAG_STD))
	summary_data
}