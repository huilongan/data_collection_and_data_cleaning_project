## Code Book 

###Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six 
activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on 
the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a 
constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly 
partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding 
windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion 
components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed 
to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features
was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Data Manipulation Technique And Corresponding Methods  

#Step 1: Merge the data

Here I used functions "rename", "mutate", "left_join" of "dplyr" and "names" and some basic data frame manipulation tools of base. Firstly,
I used read.table to load test data and training data into my R workspace. And then, I made up these two datasets respectively. And used
rbind function to put them together.

#Step 2: Select the variable

Actually, I finished it in Step 1, where I used "grep" function to subtract the pattern I summarized from the variable names, which measure
the mean and sd.

#Step 3: Clean the variable names

I used loop to subsitude the complexing part of variable names.

#Step 4: Get the Cleaned Data

Note that I added a new variable named "test_index" to represent whether it comes from test dataset.

#Step 5: Average

I mainly used functions from "reshape" package.

### Variable Description

The detailed description can be founded on the README part.
Here I list the variable in my final tidy data stored in CSV file named "clean_data.csv".
 [1] "Time.BodyAcc.mean.X"           "Time.BodyAcc.mean.Y"           "Time.BodyAcc.mean.Z"          
 [4] "Time.GravityAcc.mean.X"        "Time.GravityAcc.mean.Y"        "Time.GravityAcc.mean.Z"       
 [7] "Time.BodyAccJerk.mean.X"       "Time.BodyAccJerk.mean.Y"       "Time.BodyAccJerk.mean.Z"      
[10] "Time.BodyGyro.mean.X"          "Time.BodyGyro.mean.Y"          "Time.BodyGyro.mean.Z"         
[13] "Time.BodyGyroJerk.mean.X"      "Time.BodyGyroJerk.mean.Y"      "Time.BodyGyroJerk.mean.Z"     
[16] "Time.BodyAccMag.mean"          "Time.BodyAccMag.SD"            "Time.GravityAccMag.mean"      
[19] "Time.GravityAccMag.SD"         "Time.BodyAccJerkMag.mean"      "Time.BodyAccJerkMag.SD"       
[22] "Time.BodyGyroMag.mean"         "Time.BodyGyroMag.SD"           "Time.BodyGyroJerkMag.mean"    
[25] "Time.BodyGyroJerkMag.SD"       "Freq.BodyAcc.mean.X"           "Freq.BodyAcc.mean.Y"          
[28] "Freq.BodyAcc.mean.Z"           "Freq.BodyAcc.meanFreq.X"       "Freq.BodyAcc.meanFreq.Y"      
[31] "Freq.BodyAcc.meanFreq.Z"       "Freq.BodyAccJerk.mean.X"       "Freq.BodyAccJerk.mean.Y"      
[34] "Freq.BodyAccJerk.mean.Z"       "Freq.BodyAccJerk.meanFreq.X"   "Freq.BodyAccJerk.meanFreq.Y"  
[37] "Freq.BodyAccJerk.meanFreq.Z"   "Freq.BodyGyro.mean.X"          "Freq.BodyGyro.mean.Y"         
[40] "Freq.BodyGyro.mean.Z"          "Freq.BodyGyro.meanFreq.X"      "Freq.BodyGyro.meanFreq.Y"     
[43] "Freq.BodyGyro.meanFreq.Z"      "Freq.BodyAccMag.mean"          "Freq.BodyAccMag.SD"           
[46] "Freq.BodyAccMag.meanFreq"      "Freq.BodyAccJerkMag.mean"      "Freq.BodyAccJerkMag.SD"       
[49] "Freq.BodyAccJerkMag.meanFreq"  "Freq.BodyGyroMag.mean"         "Freq.BodyGyroMag.SD"          
[52] "Freq.BodyGyroMag.meanFreq"     "Freq.BodyGyroJerkMag.mean"     "Freq.BodyGyroJerkMag.SD"      
[55] "Freq.BodyGyroJerkMag.meanFreq" "subject"                       "act_label"                    
[58] "act_name"                      "test_index"                   

I want to explain "act_name", "act_label","subject" and "test_index":
"act_name" is the activity name. "subject" is the subject who has such a activity. "act_label" is the coded acticity type.
"test_index" is the index for whether it comes from test dataset, and "1" stands for it comes from test dataset.

The activities includes:
1 WALKING
2 WALKING_UPSTAIRS
3 WALKING_DOWNSTAIRS
4 SITTING
5 STANDING
6 LAYING

