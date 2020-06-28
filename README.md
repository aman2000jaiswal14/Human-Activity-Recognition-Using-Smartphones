# datasciencecoursera
==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

Here are the data for the project:
==================================================================
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

==================================================================
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

This repo includes the following files:
=========================================
- 'README.md'
- 'CODEBOOK.txt'
- 'run_analysis.R'
- 'tidy_data.txt'

'CODEBOOK.txt' contains column name and summary for 'tidy_data.txt'

'run_analysis.R' contains R code to convert the "Human Activity Recognition Using Smartphones Dataset" to tidy_dataset on the following factor:
     1: Merges the training and the test sets to create one data set.
     2: Extracts only the measurements on the mean and standard deviation for each measurement.
     3: Uses descriptive activity names to name the activities in the data set
     4: Appropriately labels the data set with descriptive variable names.
     5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
     
'tidy_data.txt'  tidy created in step 5 of the instructions of 'run_analysis.R'
