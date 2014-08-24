
## This function returns a list of functions. Each function object returned takes
## a file as argument and returns a vector except for "dataset" function, which
## returns a data frame.
prep_dataset <- function(directory = "UCI HAR Dataset", type) {
    prev_wd <- getwd()
    setwd(paste(directory, type, sep = "/"))
    
    ## Dynamically set filename based on type
    act_file <- paste("y_", type, ".txt", sep = "")
    subj_file <- paste("subject_", type, ".txt", sep = "")
    data_file <- paste("X_", type, ".txt", sep = "")
    
    ## Read activity, subject and dataset files
    activity <- read.table(act_file)
    subject <- read.table(subj_file)
    data    <- read.table(data_file)
    setwd(prev_wd)
    
    ## column bind subject and activity data to dataset
    cbind(subject, activity, data)
}

## This function takes as argument the directory of the raw data
##   -  binds subject ids vector from subject files (subject_{train|test}.txt)
##      and activity labels from activity files (y__{train|test}.txt) to each
##      corresponding data set.
##   -  merges resulting data set of train and test activities
##   -  uses the features vector (from features.txt) to labels the data set with
##      descriptive variable names
##   -  returns the resulting data frame, which is a merge of both data sets.
## Arguments:
##  -  directory: full/relative path of raw data set directory
merge_dataset <- function(directory = "UCI HAR Dataset") {
    ## Get variable names from features.txt
    features_file <- paste(directory, "features.txt", sep = "/")
    features <- read.table(features_file, stringsAsFactors = FALSE)
    
    ## Merge train and test datasets
    test  <- prep_dataset(type = "test")
    train <- prep_dataset(type = "train")
    merged <- rbind(train, test)

    ## Label dataset with variable names
    names(merged) <- c("subject", "activity", features[,2])
    
    merged
}


## This function extracts only the measurements on the mean and standard deviation
## for each measurement.
## Arguments:
##  -  df: merged data set
extract_mean_std <- function(df) {
    ms_cols <- grepl("mean|std", names(df))
    df[,ms_cols]
}


## This function uses descriptive activity names from "activity_labels.txt" to
## name the activities in the data set.
## Arguments:
##  -  df: data frame of merged data
##  -  directory: full/relative path of raw data set directory
label_activities <- function(df, directory = "UCI HAR Dataset") {
    ## Get activity label mappings
    al_file <- paste(directory, "activity_labels.txt", sep = "/")
    al <- read.table(al_file, col.names = c("number", "label"), stringsAsFactors = FALSE)
    
    ## Label activities with appropriate activity names
    df$activity <- factor(df$activity, labels = al$label)
    df
}

## Creates a second, independent tidy data set with the average of each variable
## for each activity and each subject.
summarize_data <- function(df) {
    library(plyr)
    ddply(df, .(subject, activity), function(x) colMeans(x[-c(1,2)]))
}

## Main function
## This function takes as argument the raw data directory and performs analysis
## on the data sets as per assignment requirements/instructions.
run_analysis <- function(directory = "UCI HAR Dataset") {
    ##   1. Merges the training and the test sets to create one data set.
    ##   3. Uses descriptive activity names to name the activities in the data set
    d <- merge_dataset(directory)
    
    ##   2. Extracts only the measurements on the mean and standard deviation for each measurement.
    e <- extract_mean_std(d)
    
    ##   4. Appropriately labels the data set with descriptive variable names.
    d <- label_activities(d, directory)
    write.table(d, file = "./merged_data.txt")
    
    ##   5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
    s <- summarize_data(d)
    write.table(d, file = "./summarized_data.txt", row.name = FALSE)
    
    list( merged_dataset = d,
          mean_std_extract = e,
          summarized_data = s )
}