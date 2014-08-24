# Code Book

## Variables
| Variable name     |  Variable label   |
| -------------     | -----------       |
| subject           | Subject identifier                            | 
| activity          | Name of activity. The selection of activities are given below  |    
| tBodyAcc-XYZ      | Body acceleration time domain signals         |
| tGravityAcc-XYZ   | Gravity acceleration time domain signals      |
| tBodyAccJerk-XYZ  | Body acceleration time domain Jerk signals    |
| tBodyGyro-XYZ     | Body gyroscope time domain signals            |
| tBodyGyroJerk-XYZ | Body gyroscope time domain Jerk signals       |
| tBodyAccMag       | Body acceleration time domain Mag signals     |
| tGravityAccMag    | Gravity acceleration time domain Mag signals  |
| tBodyAccJerkMag   | Body acceleration time domain Jerk-Mag signals |
| tBodyGyroMag      | Body gyroscope time domain Mag signals        |
| tBodyGyroJerkMag  | Body gyroscope time domain Jerk-Mag signals   |
| fBodyAcc-XYZ      | Body acceleration frequency domain signals    |
| fBodyAccJerk-XYZ  | Body acceleration frequency domain Jerk signals   |
| fBodyGyro-XYZ     | Body gyroscope frequency domain signals       |
| fBodyAccMag       | Body acceleration frequency domain Mag signals |
| fBodyAccJerkMag   | Body acceleration frequency domain Jerk-Mag signals |
| fBodyGyroMag      | Body gyroscope frequency domain Mag signals   |
| fBodyGyroJerkMag  | Body gyroscope frequency domain Jerk-Mag signals |


The set of variables that were estimated from the above signals are: 


| Variable name |  Variable label   |
| ------------- | -----------       |
| mean()        | Mean value                |
| std()         | Standard deviation        |
| mad()         | Median absolute deviation |
| max()         | Largest value in array    |
| min()         | Smallest value in array   |
| sma()         | Signal magnitude area     |
| energy()      | Energy measure. Sum of the squares divided by the number of values. |
| iqr()         | Interquartile range       |
| entropy()     | Signal entropy            |
| arCoeff()     | Autorregresion coefficients with Burg order equal to 4    |
| correlation() | correlation coefficient between two signals               |
| maxInds()     | index of the frequency component with largest magnitude   |
| meanFreq()    | Weighted average of the frequency components to obtain a mean frequency |
| skewness()    | skewness of the frequency domain signal   |
| kurtosis()    | kurtosis of the frequency domain signal   |
| bandsEnergy() | Energy of a frequency interval within the 64 bins of the FFT of each window.
| angle()       | Angle between to vectors.                 |


Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

- gravityMean
- tBodyAccMean
- tBodyAccJerkMean
- tBodyGyroMean
- tBodyGyroJerkMean

Please refer to features.txt and features_info.txt for further details.


## Activity label


| Value | Label |
| ----- | ----- |
| 1 | WALKING           |
| 2 | WALKING_UPSTAIRS  |
| 3 | WALKING_DOWNSTAIRS |
| 4 | SITTING           |
| 5 | STANDING          |
| 6 | LAYING            |

