library(data.table)
library(reshape2)  # faster melt and dcast functions

# COMMON data processing ===========================================================================
# Set the Working Directory.  The data should be in subfolders from here.
# setwd("~/Documents/Coursera/01-03 Getting And Cleaning Data/Project")

# Read in the activity labels
dt.ActivityLabels <- fread( file.path("UCI HAR Dataset","activity_labels.txt"),
                            col.names = c("activity_id","activity_desc"), data.table = TRUE)

# Read in the Features (i.e. 561 variable definitions) used for each observation.
dt.Features <- fread( file.path("UCI HAR Dataset","features.txt"),
                      col.names = c("column_id","column_name"), data.table = TRUE)

# Determine which variables are either the 'mean()' or 'std()'.
dt.ColumnsNeeded <- dt.Features[grep("mean()|std()",dt.Features$column_name)]

# TEST dataset processing ==========================================================================#
#
# Read in the test subjects
dt.TestSubjects <- fread( file.path("UCI HAR Dataset","test","Subject_test.txt"),
                         col.names = c("subject_id"), data.table = TRUE)

# Read in the test activity ids
dt.TestActivityId <- fread( file.path("UCI HAR Dataset","test","y_test.txt"),
                            col.names = c("activity_id"), data.table = TRUE)

# Read in the test data.  Note we only keep the variables for mean() or std().
df.TestData <- fread( file.path("UCI HAR Dataset","test","X_test.txt"),
                      data.table = FALSE)[,dt.ColumnsNeeded$column_id]
colnames(df.TestData) <- as.vector(dt.ColumnsNeeded$column_name)

# Bind all of the variables (columns) together
df.AllTestData <- cbind( dt.TestSubjects, dt.TestActivityId, df.TestData)

# TRAIN dataset processing =========================================================================
#
# Read in the train subjects
dt.TrainSubjects <- fread( file.path("UCI HAR Dataset","train","Subject_train.txt"),
                         col.names = c("subject_id"), data.table = TRUE)

# Read in the train activity ids
dt.TrainActivityId <- fread( file.path("UCI HAR Dataset","train","y_train.txt"),
                            col.names = c("activity_id"), data.table = TRUE)

# Read in the test data.  Note we only keep the variables for mean() or std().
df.TrainData <- fread( file.path("UCI HAR Dataset","train","X_train.txt"),
                      data.table = FALSE)[,dt.ColumnsNeeded$column_id]
colnames(df.TrainData) <- as.vector(dt.ColumnsNeeded$column_name)

# Bind all of the variables (columns) together
df.AllTrainData <- cbind( dt.TrainSubjects, dt.TrainActivityId, df.TrainData)

# Bind the observations (rows) between TEST and TRAIN
df.AllData <- rbind( df.AllTestData, df.AllTrainData)

# Merge in the Activity Description 'activity_desc'; link on the activity_id in both.
df.AllData <- merge( df.AllData, dt.ActivityLabels, by.x="activity_id", by.y="activity_id", all.x=TRUE)
# remove the activity_id variable (using the fast method) as we have the activity_desc now.
df.AllData[,activity_id:=NULL]

# Once compete, remove unneeded data stores.
# Comment the following rm()'s to see results from individual steps
# rm(df.AllTestData)
# rm(df.AllTrainData)
# rm(df.TestData)
# rm(df.TrainData)
# rm(dt.ActivityLabels)
# rm(dt.ColumnsNeeded)
# rm(dt.Features)
# rm(dt.TestActivityId)
# rm(dt.TestSubjects)
# rm(dt.TrainActivityId)
# rm(dt.TrainSubjects)

# TIDY data processing here ============================================================
# First step is to melt the data such that the result is one observation for each subject,
# activity, and variable per observation and each observation has only one variable (i.e. value).
df.TidyData <- melt(df.AllData, id.vars=c("subject_id","activity_desc"))

# STEP 5: Create a data set with the MEAN of each VARIABLE for each SUBJECT and ACTIVITY
df.TidySum<- dcast( df.TidyData, subject_id + activity_desc ~ variable,
                    fun.aggregate=mean )

# write the file for submittal
write.table(df.TidySum, file= "TidySum.txt", row.names = FALSE)

# To read what the write.table() above did.
# data <- read.table("TidySum.txt",header=TRUE)
# View(data)