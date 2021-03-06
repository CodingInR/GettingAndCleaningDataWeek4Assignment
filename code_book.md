# Original data set decriptions
* Found in the UCI data set when downloaded and unzipped
* Original code books only breifly described here, this code book only goes into detail on data transformation performed by the script

## Files (original)
* activity_lables.txt : translation of coded integers to activity (descriptive string)
* data.zip : The data
  * test and training data. X signifies the data, Y signifies the activity, subject identifies the subject by number
* features.txt : lists the recorded sensor data (w/ some additional calculations) variable names
* features_info.txt : goes into detail on what the recorded sensor data is
* README.txt : summarizes the data

## Overview of the run_analysis.R transformation steps (further details in script comments)
* Installs necesary packages/libraries
* Downloads and unzips the data (after checking whether it has already been downloaded)
* Loads the following source data files into data frames
  * subject_train.txt
  * subject_test.tet
  * features.txt
  * activity_lables.txt
  * X_test.txt
  * y_test.txt
  * X_train.txt
  * y_train.txt
* Combines the feature data, the activity data and the subject data respectively by row-binding training and test data
* Combines the three files in prior step by column binding (subject, activity, data)
* Names the columns ("subject", "activity", *feature names*)
  * *feature names* is a 561 element character vector transposed from the second colum of features.txt
* Changes the numbered activity variables to descriptions (e.g. "1" to "WALKING") by factorizing activity lables into the activity column
* Using grepl to search for means and standard deviations (and including subject and activity), the data is subsetted into a smaller data set
  * *Note: includes frequency means
* A tidy data set is produced by using dplyr to redice the data, grouped by activity and subject and summarized by means
* The tidy data set is written out to a text file ("tidy_data.txt"), provided in this repo
