# GCD_CP
Getting and Cleaning Data - Course Project

This repo contains run_analysis.R which is needed for Course Project for Gettting and Cleaning Data course on Coursera.

Working directory should be set to UCI HAR Dataset.

run_analysis.R consits of two functions:

1. clean_data()
  1. Takes train and test data as input and loads them into one big dataset.
  2. Loads variable names, gets rid of the duplicate variable names, and then selects only variable from data set which contain strings "mean" and "std".
  3. Loads subjects from test and train data, binds them.
  4. Loads test and train activities, binds them and merges them with activity names.
  5. Cbinds subjects and activites to original data frame.
  6. Properly formats all variable names so they are easier to read and reference.
  7. Groups dataset by subject ID (SUB_ID) and activity ID (ACT_ID).
  8. Returns dataset.

2. summarize_data()
  1. Takes clean_data() output as input.
  2. Summarizes data by subject and activity and calculates mean of variables from UCI HAR dataset.
  3. Returns summarized data.

Example usage:

>source("run_analysis.R")
>cleaneddata <- clean_data()
>summarizeddata <- summarize_data(cleaneddata)