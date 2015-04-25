# Load the packages
packages <- c("data.table", "reshape2")
sapply(packages, require, character.only=TRUE, quietly=TRUE)

warnPath <- function(path) {
  if(!file.exists(path)) {
    msg <- paste("Please use setwd() to the root of this project. File: ",path,"not found")
    stop(msg)
  }
}

loadMergeData <- function(trainSet, testSet, variableName) {
  trainingPath <- file.path(path, "train", trainSet)
  warnPath(trainingPath)
  trainingSubjects <- fread(trainingPath)
  testPath <- file.path(path, "test" , testSet )
  warnPath(testPath)
  testSubjects  <- fread(testPath)
  subjects <- rbind(trainingSubjects, testSubjects)
  setnames(subjects, "V1", variableName)  
}

#Assumes this script lies in the UCI HAR Dataset
path <- getwd()
path <- file.path(path, "data")

# Read in and merge subjects
subjects <- loadMergeData("subject_train.txt","subject_test.txt", "subject")

# Read in and merge activities
activities <- loadMergeData("y_train.txt", "y_test.txt", "activityNumber")

# Read in and merge measurements
mpath <- file.path(path, "train", "X_train.txt")
warnPath(mpath)
trainingMeasures <- data.table(read.table(mpath))
mpath <- file.path(path, "test" , "X_test.txt")
warnPath(mpath)
testMeasures  <- data.table(read.table(mpath))
measures <- rbind(trainingMeasures, testMeasures)

# Column merge the subjects to activities
subjectActivities <- cbind(subjects, activities)
subjectActvitiesWithMeasures <- cbind(subjectActivities, measures)
# Order all of the combined data by, subject and activity
setkey(subjectActvitiesWithMeasures, subject, activityNumber)

# Get feature names
allFeatures <- fread(file.path(path, "features.txt"))
setnames(allFeatures, c("V1", "V2"), c("measureNumber", "measureName"))

# Use grepl to just get features/measures related to mean and std
meanStdMeasures <- allFeatures[grepl("(mean|std)\\(\\)", measureName)]
meanStdMeasures$measureColumn <- meanStdMeasures[, paste0("V", measureNumber)]

# Create list with columns to select from subjectActivitiesWithMeasures
columnsToSelect <- c(key(subjectActvitiesWithMeasures), meanStdMeasures$measureColumn)
# Extract the columns of interest ( std() and mean() )
subjectActivitesWithMeasuresMeanStd <- subset(subjectActvitiesWithMeasures, 
                                                select = columnsToSelect)

# Read in the activity names and give them more meaningful names
activityNames <- fread(file.path(path, "activity_labels.txt"))
setnames(activityNames, c("V1", "V2"), c("activityNumber", "activityName"))

# Merge the 'meaningful activity names' with the 
# subjectActiitiesWithMeasuresMeanStd
subjectActivitesWithMeasuresMeanStd <- merge(subjectActivitesWithMeasuresMeanStd, 
                                               activityNames, by = "activityNumber", 
                                               all.x = TRUE)

# Sort the subjectActivitesWithMeasuresMeanStd
setkey(subjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName)

# Convert from a wide to narrow data.table using the keys created earlier
subjectActivitesWithMeasuresMeanStd <- data.table(melt(subjectActivitesWithMeasuresMeanStd, 
                                                         id=c("subject", "activityName"), 
                                                         measure.vars = c(3:68), 
                                                         variable.name = "measureColumn", 
                                                         value.name="measureValue"))

# Add measure columns
subjectActivitesWithMeasuresMeanStd <- merge(subjectActivitesWithMeasuresMeanStd, 
                                               meanStdMeasures[, list(measureNumber, measureColumn, measureName)], 
                                              by="measureColumn", all.x=TRUE)

# Reshape the data to get the averages 
measureAvgerages <- dcast(subjectActivitesWithMeasuresMeanStd, 
                          subject + activityName ~ measureName, 
                          mean, 
                          value.var="measureValue")

# Write the tab delimited file
write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t")