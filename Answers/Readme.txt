
-- I have used open source Language R. A library called Rattle which has a front end to run algorithms and clean data. The log files which has some codes are presented below.

	Buy_or_not Column generation Log

# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-07-31 18:31:56 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'MAHE'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-07-31 18:32:02 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 18:32:02 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:32:17 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 1*crs$nobs) # 31418 observations
crs$validate <- NULL
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:32:35 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Buy_or_not ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 1.99 mins

#============================================================
# Rattle timestamp: 2016-07-31 18:37:37 x86_64-w64-mingw32 

# Evaluate model performance. 

# Read a dataset from file for testing the model.

crs$testset <- read.csv("D:/Contests/ZS Associates/Models/Solution_revenue_score_all.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Generate an Error Matrix for the Ada Boost model.

# Obtain the response from the Ada Boost model.

crs$pr <- predict(crs$ada, newdata=crs$testset[,c(crs$input, crs$target)])

#============================================================
# Rattle timestamp: 2016-07-31 18:39:06 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 18:39:07 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21997 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4713 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4715 observations

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:39:16 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 1*crs$nobs) # 31425 observations
crs$validate <- NULL
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:39:22 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Buy_or_not ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 20.58 secs

#============================================================
# Rattle timestamp: 2016-07-31 18:43:52 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 1*crs$nobs) # 31425 observations
crs$validate <- NULL
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:43:58 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds1.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 18:44:01 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Average",
     "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg",
     "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average",
     "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:44:07 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 18:44:08 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21997 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4713 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4715 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:44:16 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 1*crs$nobs) # 31425 observations
crs$validate <- NULL
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:44:23 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Buy_or_not ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 20.59 secs

#============================================================
# Rattle timestamp: 2016-07-31 18:45:48 x86_64-w64-mingw32 

# Score a dataset. 

# Read a dataset from file for testing the model.

crs$testset <- read.csv("D:/Contests/ZS Associates/Models/Solution_revenue_score_all.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Add missing column `Hospital_ID' to the testing dataset

crs$testset <- cbind(crs$testset[1:0], Hospital_ID=rep(NA, nrow(crs$testset)), crs$testset[1:ncol(crs$testset)])

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Obtain probability scores for the Ada Boost model on Solution_revenue_score_all.csv.

crs$pr <- predict(crs$ada, newdata=crs$testset[,c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$testset[,], select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\Solution_revenue_score_all_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2016-07-31 18:47:42 x86_64-w64-mingw32 

# Score a dataset. 

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Obtain probability scores for the Ada Boost model on ds2.csv.

crs$pr <- predict(crs$ada, newdata=crs$testset[,c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$testset[,], select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\ds2_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2016-07-31 18:48:52 x86_64-w64-mingw32 

# Score a dataset. 

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Obtain probability scores for the Ada Boost model on ds2.csv.

crs$pr <- predict(crs$ada, newdata=crs$dataset)

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset, select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\ds2_score_idents_ignore.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2016-07-31 18:49:46 x86_64-w64-mingw32 

# Score a dataset. 

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Obtain probability scores for the Ada Boost model on ds2.csv.

crs$pr <- predict(crs$ada, newdata=crs$testset[,c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$testset[,], select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\ds2_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2016-07-31 18:50:10 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds2.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 18:50:11 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21997 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4713 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4715 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:50:20 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31425 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 1*crs$nobs) # 31425 observations
crs$validate <- NULL
crs$test <- NULL

# The following variable selections have been noted.

crs$input <- c("District_ID", "Instrument_ID", "Annual_Projected_Revenue")

crs$numeric <- "Annual_Projected_Revenue"

crs$categoric <- c("District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- "Hospital_ID"
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 18:50:39 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Buy_or_not ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 20.22 secs

#============================================================
# Rattle timestamp: 2016-07-31 18:51:38 x86_64-w64-mingw32 

# Score a dataset. 

# Read a dataset from file for testing the model.

crs$testset <- read.csv("D:/Contests/ZS Associates/Models/Solution_revenue_score_all.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Add missing column `Hospital_ID' to the testing dataset

crs$testset <- cbind(crs$testset[1:0], Hospital_ID=rep(NA, nrow(crs$testset)), crs$testset[1:ncol(crs$testset)])

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

# Obtain probability scores for the Ada Boost model on Solution_revenue_score_all.csv.

crs$pr <- predict(crs$ada, newdata=crs$testset[,c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$testset[,], select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\Solution_revenue_score_all_score_idents.csv", row.names=FALSE)


# Predicted Revenue Log

# Rattle is Copyright (c) 2006-2015 Togaware Pty Ltd.

#============================================================
# Rattle timestamp: 2016-07-31 02:29:16 x86_64-w64-mingw32 

# Rattle version 4.1.0 user 'MAHE'

# This log file captures all Rattle interactions as R commands. 

Export this log to a file using the Export button or the Tools 
# menu to save a log of all your activity. This facilitates repeatability. For example, exporting 
# to a file called 'myrf01.R' will allow you to type in the R Console 
# the command source('myrf01.R') and so repeat all actions automatically. 
# Generally, you will want to edit the file to suit your needs. You can also directly 
# edit this current log in place to record additional information before exporting. 
 
# Saving and loading projects also retains this log.

# We begin by loading the required libraries.

library(rattle)   # To access the weather dataset and utility commands.
library(magrittr) # For the %>% and %<>% operators.

# This log generally records the process of building a model. However, with very 
# little effort the log can be used to score a new dataset. The logical variable 
# 'building' is used to toggle between generating transformations, as when building 
# a model, and simply using the transformations, as when scoring a dataset.

building <- TRUE
scoring  <- ! building


# A pre-defined value is used to reset the random seed so that results are repeatable.

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2016-07-31 02:29:43 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/dataset.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 02:29:46 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "Min.Year",
     "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum", "Fourth.Quarter.Sum",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total", "Second.Half.Year.Average",
     "Hospital_employees", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "Min.Year", "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum",
     "Fourth.Quarter.Sum", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg",
     "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total",
     "Second.Half.Year.Average", "Hospital_employees", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:29:53 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "Min.Year",
     "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum", "Fourth.Quarter.Sum",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total", "Second.Half.Year.Average",
     "Hospital_employees", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "Min.Year", "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum",
     "Fourth.Quarter.Sum", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg",
     "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total",
     "Second.Half.Year.Average", "Hospital_employees", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:30:13 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation dataset.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2016-07-31 02:31:29 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "Min.Year",
     "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum", "Fourth.Quarter.Sum",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total", "Second.Half.Year.Average",
     "Hospital_employees", "Annual_Projected_Revenue", "Buy_or_not")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "Min.Year", "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum",
     "Fourth.Quarter.Sum", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg",
     "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total",
     "Second.Half.Year.Average", "Hospital_employees", "Annual_Projected_Revenue", "Buy_or_not")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:31:37 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation dataset.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2016-07-31 02:48:08 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds1.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 02:48:10 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:48:24 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue", "Buy_or_not")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue",
     "Buy_or_not")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:48:29 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ds1.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2016-07-31 02:50:21 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue", "Buy_or_not")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue",
     "Buy_or_not")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:50:45 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/dataset.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 02:50:47 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Total",
     "Year.Average", "Year.Median", "Max.Year", "Min.Year",
     "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum", "Fourth.Quarter.Sum",
     "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg",
     "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total", "Second.Half.Year.Average",
     "Hospital_employees", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Total", "Year.Average", "Year.Median", "Max.Year",
     "Min.Year", "First.Quarter.Sum", "Second.Quarter.Sum", "Third.Quarter.Sum",
     "Fourth.Quarter.Sum", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg",
     "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Total",
     "Second.Half.Year.Average", "Hospital_employees", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:50:52 x86_64-w64-mingw32 

# Load the data.

crs$dataset <- read.csv("file:///D:/Contests/ZS Associates/Churning/ds1.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#============================================================
# Rattle timestamp: 2016-07-31 02:50:55 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 4712 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 4714 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Average",
     "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg",
     "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average",
     "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:51:05 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Average",
     "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg",
     "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average",
     "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:51:13 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Average",
     "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg",
     "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average",
     "Second.Half.Year.Average", "Annual_Projected_Revenue", "Buy_or_not")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue", "Buy_or_not")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:51:17 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ds1.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2016-07-31 02:53:42 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Month.1",
     "Month.2", "Month.3", "Month.4", "Month.5",
     "Month.6", "Month.7", "Month.8", "Month.9",
     "Month.10", "Month.11", "Month.12", "Year.Average",
     "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg",
     "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average",
     "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$numeric <- c("Month.1", "Month.2", "Month.3", "Month.4",
     "Month.5", "Month.6", "Month.7", "Month.8",
     "Month.9", "Month.10", "Month.11", "Month.12",
     "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg",
     "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total",
     "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- NULL
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:54:32 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

correlationTest(na.omit(crs$dataset[, "Annual_Projected_Revenue"]), na.omit(crs$dataset[, "Max.Year"]))

#============================================================
# Rattle timestamp: 2016-07-31 02:55:12 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

locationTest(na.omit(crs$dataset[crs$dataset[["Buy_or_not"]] == "0", "Annual_Projected_Revenue"]), na.omit(crs$dataset[crs$dataset[["Buy_or_not"]] == "1", "Annual_Projected_Revenue"]))

#============================================================
# Rattle timestamp: 2016-07-31 02:56:03 x86_64-w64-mingw32 

# Transform variables by rescaling. 

# Rescale Month.1.

crs$dataset[["RRC_Month.1"]] <- crs$dataset[["Month.1"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.1"]] <-
    scale(crs$dataset[["Month.1"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.1"]] <- (crs$dataset[["Month.1"]] - 2134.334108)/14366.447689
}

# Rescale Month.2.

crs$dataset[["RRC_Month.2"]] <- crs$dataset[["Month.2"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.2"]] <-
    scale(crs$dataset[["Month.2"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.2"]] <- (crs$dataset[["Month.2"]] - 2184.365396)/12297.962785
}

# Rescale Month.3.

crs$dataset[["RRC_Month.3"]] <- crs$dataset[["Month.3"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.3"]] <-
    scale(crs$dataset[["Month.3"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.3"]] <- (crs$dataset[["Month.3"]] - 2279.572061)/13543.684378
}

# Rescale Month.4.

crs$dataset[["RRC_Month.4"]] <- crs$dataset[["Month.4"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.4"]] <-
    scale(crs$dataset[["Month.4"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.4"]] <- (crs$dataset[["Month.4"]] - 2212.818289)/13147.606301
}

# Rescale Month.5.

crs$dataset[["RRC_Month.5"]] <- crs$dataset[["Month.5"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.5"]] <-
    scale(crs$dataset[["Month.5"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.5"]] <- (crs$dataset[["Month.5"]] - 2347.533325)/15021.677293
}

# Rescale Month.6.

crs$dataset[["RRC_Month.6"]] <- crs$dataset[["Month.6"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.6"]] <-
    scale(crs$dataset[["Month.6"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.6"]] <- (crs$dataset[["Month.6"]] - 2327.597205)/14419.187460
}

# Rescale Month.7.

crs$dataset[["RRC_Month.7"]] <- crs$dataset[["Month.7"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.7"]] <-
    scale(crs$dataset[["Month.7"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.7"]] <- (crs$dataset[["Month.7"]] - 2336.817748)/14651.458704
}

# Rescale Month.8.

crs$dataset[["RRC_Month.8"]] <- crs$dataset[["Month.8"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.8"]] <-
    scale(crs$dataset[["Month.8"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.8"]] <- (crs$dataset[["Month.8"]] - 2231.788624)/13919.643327
}

# Rescale Month.9.

crs$dataset[["RRC_Month.9"]] <- crs$dataset[["Month.9"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.9"]] <-
    scale(crs$dataset[["Month.9"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.9"]] <- (crs$dataset[["Month.9"]] - 2261.085683)/13735.038276
}

# Rescale Month.10.

crs$dataset[["RRC_Month.10"]] <- crs$dataset[["Month.10"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.10"]] <-
    scale(crs$dataset[["Month.10"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.10"]] <- (crs$dataset[["Month.10"]] - 2213.452193)/13251.290417
}

# Rescale Month.11.

crs$dataset[["RRC_Month.11"]] <- crs$dataset[["Month.11"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.11"]] <-
    scale(crs$dataset[["Month.11"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.11"]] <- (crs$dataset[["Month.11"]] - 2690.117608)/18972.485361
}

# Rescale Month.12.

crs$dataset[["RRC_Month.12"]] <- crs$dataset[["Month.12"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Month.12"]] <-
    scale(crs$dataset[["Month.12"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Month.12"]] <- (crs$dataset[["Month.12"]] - 2320.150487)/14526.112208
}

# Rescale Year.Average.

crs$dataset[["RRC_Year.Average"]] <- crs$dataset[["Year.Average"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Year.Average"]] <-
    scale(crs$dataset[["Year.Average"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Year.Average"]] <- (crs$dataset[["Year.Average"]] - 2294.969381)/12611.384402
}

# Rescale Year.Median.

crs$dataset[["RRC_Year.Median"]] <- crs$dataset[["Year.Median"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Year.Median"]] <-
    scale(crs$dataset[["Year.Median"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Year.Median"]] <- (crs$dataset[["Year.Median"]] - 2184.579318)/13085.115226
}

# Rescale Max.Year.

crs$dataset[["RRC_Max.Year"]] <- crs$dataset[["Max.Year"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Max.Year"]] <-
    scale(crs$dataset[["Max.Year"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Max.Year"]] <- (crs$dataset[["Max.Year"]] - 4561.212140)/22715.469093
}

# Rescale First.Quarter.Avg.

crs$dataset[["RRC_First.Quarter.Avg"]] <- crs$dataset[["First.Quarter.Avg"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_First.Quarter.Avg"]] <-
    scale(crs$dataset[["First.Quarter.Avg"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_First.Quarter.Avg"]] <- (crs$dataset[["First.Quarter.Avg"]] - 2199.423826)/12185.007567
}

# Rescale Second.Quarter.Avg.

crs$dataset[["RRC_Second.Quarter.Avg"]] <- crs$dataset[["Second.Quarter.Avg"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Second.Quarter.Avg"]] <-
    scale(crs$dataset[["Second.Quarter.Avg"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Second.Quarter.Avg"]] <- (crs$dataset[["Second.Quarter.Avg"]] - 2295.982955)/13167.998906
}

# Rescale Third.Quarter.Avg.

crs$dataset[["RRC_Third.Quarter.Avg"]] <- crs$dataset[["Third.Quarter.Avg"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Third.Quarter.Avg"]] <-
    scale(crs$dataset[["Third.Quarter.Avg"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Third.Quarter.Avg"]] <- (crs$dataset[["Third.Quarter.Avg"]] - 2276.564018)/13453.601935
}

# Rescale Fourth.Quarter.Avg.

crs$dataset[["RRC_Fourth.Quarter.Avg"]] <- crs$dataset[["Fourth.Quarter.Avg"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Fourth.Quarter.Avg"]] <-
    scale(crs$dataset[["Fourth.Quarter.Avg"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Fourth.Quarter.Avg"]] <- (crs$dataset[["Fourth.Quarter.Avg"]] - 2407.906753)/14529.569150
}

# Rescale First.Half.Year.Total.

crs$dataset[["RRC_First.Half.Year.Total"]] <- crs$dataset[["First.Half.Year.Total"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_First.Half.Year.Total"]] <-
    scale(crs$dataset[["First.Half.Year.Total"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_First.Half.Year.Total"]] <- (crs$dataset[["First.Half.Year.Total"]] - 13486.220383)/73663.943868
}

# Rescale First.Half.Year.Average.

crs$dataset[["RRC_First.Half.Year.Average"]] <- crs$dataset[["First.Half.Year.Average"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_First.Half.Year.Average"]] <-
    scale(crs$dataset[["First.Half.Year.Average"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_First.Half.Year.Average"]] <- (crs$dataset[["First.Half.Year.Average"]] - 14053.412343)/82076.379441
}

# Rescale Second.Half.Year.Average.

crs$dataset[["RRC_Second.Half.Year.Average"]] <- crs$dataset[["Second.Half.Year.Average"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Second.Half.Year.Average"]] <-
    scale(crs$dataset[["Second.Half.Year.Average"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Second.Half.Year.Average"]] <- (crs$dataset[["Second.Half.Year.Average"]] - 2340.144197)/13617.152008
}

# Rescale Annual_Projected_Revenue.

crs$dataset[["RRC_Annual_Projected_Revenue"]] <- crs$dataset[["Annual_Projected_Revenue"]]

# Recenter and rescale the data around 0.

if (building)
{
  crs$dataset[["RRC_Annual_Projected_Revenue"]] <-
    scale(crs$dataset[["Annual_Projected_Revenue"]])[,1]
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RRC_Annual_Projected_Revenue"]] <- (crs$dataset[["Annual_Projected_Revenue"]] - 18166.412502)/185664.094964
}

#============================================================
# Rattle timestamp: 2016-07-31 02:56:04 x86_64-w64-mingw32 

# Note the user selections. 

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "RRC_Month.1",
     "RRC_Month.2", "RRC_Month.3", "RRC_Month.4", "RRC_Month.5",
     "RRC_Month.6", "RRC_Month.7", "RRC_Month.8", "RRC_Month.9",
     "RRC_Month.10", "RRC_Month.11", "RRC_Month.12", "RRC_Year.Average",
     "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg", "RRC_Second.Quarter.Avg",
     "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total", "RRC_First.Half.Year.Average",
     "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$numeric <- c("RRC_Month.1", "RRC_Month.2", "RRC_Month.3", "RRC_Month.4",
     "RRC_Month.5", "RRC_Month.6", "RRC_Month.7", "RRC_Month.8",
     "RRC_Month.9", "RRC_Month.10", "RRC_Month.11", "RRC_Month.12",
     "RRC_Year.Average", "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg",
     "RRC_Second.Quarter.Avg", "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total",
     "RRC_First.Half.Year.Average", "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:56:27 x86_64-w64-mingw32 

# View the dataset. 

# The 'RGtk2Extras' package provides the 'dfedit' function.

library(RGtk2Extras)

# Please note that any edits will be ignored.

RGtk2Extras::dfedit(crs$dataset,
                    dataset.name="Rattle Dataset",
                    size=c(800, 400))

#============================================================
# Rattle timestamp: 2016-07-31 02:57:19 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "RRC_Month.1",
     "RRC_Month.2", "RRC_Month.3", "RRC_Month.4", "RRC_Month.5",
     "RRC_Month.6", "RRC_Month.7", "RRC_Month.8", "RRC_Month.9",
     "RRC_Month.10", "RRC_Month.11", "RRC_Month.12", "RRC_Year.Average",
     "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg", "RRC_Second.Quarter.Avg",
     "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total", "RRC_First.Half.Year.Average",
     "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$numeric <- c("RRC_Month.1", "RRC_Month.2", "RRC_Month.3", "RRC_Month.4",
     "RRC_Month.5", "RRC_Month.6", "RRC_Month.7", "RRC_Month.8",
     "RRC_Month.9", "RRC_Month.10", "RRC_Month.11", "RRC_Month.12",
     "RRC_Year.Average", "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg",
     "RRC_Second.Quarter.Avg", "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total",
     "RRC_First.Half.Year.Average", "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:57:39 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ds1.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2016-07-31 02:58:20 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "Buy_or_not",
     "RRC_Month.1", "RRC_Month.2", "RRC_Month.3", "RRC_Month.4",
     "RRC_Month.5", "RRC_Month.6", "RRC_Month.7", "RRC_Month.8",
     "RRC_Month.9", "RRC_Month.10", "RRC_Month.11", "RRC_Month.12",
     "RRC_Year.Average", "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg",
     "RRC_Second.Quarter.Avg", "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total",
     "RRC_First.Half.Year.Average", "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$numeric <- c("Buy_or_not", "RRC_Month.1", "RRC_Month.2", "RRC_Month.3",
     "RRC_Month.4", "RRC_Month.5", "RRC_Month.6", "RRC_Month.7",
     "RRC_Month.8", "RRC_Month.9", "RRC_Month.10", "RRC_Month.11",
     "RRC_Month.12", "RRC_Year.Average", "RRC_Year.Median", "RRC_Max.Year",
     "RRC_First.Quarter.Avg", "RRC_Second.Quarter.Avg", "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg",
     "RRC_First.Half.Year.Total", "RRC_First.Half.Year.Average", "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- NULL
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 02:58:23 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ds1.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2016-07-31 03:00:13 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 2.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 2)

#============================================================
# Rattle timestamp: 2016-07-31 03:00:15 x86_64-w64-mingw32 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 1.15 secs

#============================================================
# Rattle timestamp: 2016-07-31 03:01:09 x86_64-w64-mingw32 

# Generate statistics for the clustering. 

# The 'fpc' package provides the 'cluster.stats' function.

library(fpc, quietly=TRUE)

#============================================================
# Rattle timestamp: 2016-07-31 03:01:28 x86_64-w64-mingw32 

# Display a scatterplot matrix for the KMeans clustering. 

#============================================================
# Rattle timestamp: 2016-07-31 03:02:05 x86_64-w64-mingw32 

# Note the user selections. 

# Build the training/validate/test datasets.

set.seed(crv$seed) 
crs$nobs <- nrow(crs$dataset) # 31418 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 21992 observations
crs$validate <- NULL
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 9426 observations

# The following variable selections have been noted.

crs$input <- c("Hospital_ID", "District_ID", "Instrument_ID", "RRC_Month.1",
     "RRC_Month.2", "RRC_Month.3", "RRC_Month.4", "RRC_Month.5",
     "RRC_Month.6", "RRC_Month.7", "RRC_Month.8", "RRC_Month.9",
     "RRC_Month.10", "RRC_Month.11", "RRC_Month.12", "RRC_Year.Average",
     "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg", "RRC_Second.Quarter.Avg",
     "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total", "RRC_First.Half.Year.Average",
     "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$numeric <- c("RRC_Month.1", "RRC_Month.2", "RRC_Month.3", "RRC_Month.4",
     "RRC_Month.5", "RRC_Month.6", "RRC_Month.7", "RRC_Month.8",
     "RRC_Month.9", "RRC_Month.10", "RRC_Month.11", "RRC_Month.12",
     "RRC_Year.Average", "RRC_Year.Median", "RRC_Max.Year", "RRC_First.Quarter.Avg",
     "RRC_Second.Quarter.Avg", "RRC_Third.Quarter.Avg", "RRC_Fourth.Quarter.Avg", "RRC_First.Half.Year.Total",
     "RRC_First.Half.Year.Average", "RRC_Second.Half.Year.Average", "RRC_Annual_Projected_Revenue")

crs$categoric <- c("Hospital_ID", "District_ID", "Instrument_ID")

crs$target  <- "Buy_or_not"
crs$risk    <- NULL
crs$ident   <- NULL
crs$ignore  <- c("Month.1", "Month.2", "Month.3", "Month.4", "Month.5", "Month.6", "Month.7", "Month.8", "Month.9", "Month.10", "Month.11", "Month.12", "Year.Average", "Year.Median", "Max.Year", "First.Quarter.Avg", "Second.Quarter.Avg", "Third.Quarter.Avg", "Fourth.Quarter.Avg", "First.Half.Year.Total", "First.Half.Year.Average", "Second.Half.Year.Average", "Annual_Projected_Revenue")
crs$weights <- NULL

#============================================================
# Rattle timestamp: 2016-07-31 03:02:14 x86_64-w64-mingw32 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 2.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"), 2)

#============================================================
# Rattle timestamp: 2016-07-31 03:02:14 x86_64-w64-mingw32 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$sample, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.48 secs

#============================================================
# Rattle timestamp: 2016-07-31 03:02:23 x86_64-w64-mingw32 

# Display a scatterplot matrix for the KMeans clustering. 

#============================================================
# Rattle timestamp: 2016-07-31 03:02:44 x86_64-w64-mingw32 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Buy_or_not ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0))

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.74 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

#============================================================
# Rattle timestamp: 2016-07-31 03:02:56 x86_64-w64-mingw32 

# Plot the resulting Decision Tree. 

# We use the rpart.plot package.

fancyRpartPlot(crs$rpart, main="Decision Tree ds1.csv $ Buy_or_not")

#============================================================
# Rattle timestamp: 2016-07-31 03:05:50 x86_64-w64-mingw32 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Buy_or_not ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=30,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 24.61 secs

# Display tree number 1.

drawTreesAda(crs$ada, 1, ": ds1.csv $ Buy_or_not")

#============================================================
# Rattle timestamp: 2016-07-31 03:08:47 x86_64-w64-mingw32 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Buy_or_not) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 10.11 mins

#============================================================
# Rattle timestamp: 2016-07-31 03:20:22 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(Buy_or_not ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    family=binomial(link="logit"))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat(sprintf("Log likelihood: %.3f (%d df)\n",
            logLik(crs$glm)[1],
            attr(logLik(crs$glm), "df")))
cat(sprintf("Null/Residual deviance difference: %.3f (%d df)\n",
            crs$glm$null.deviance-crs$glm$deviance,
            crs$glm$df.null-crs$glm$df.residual))
cat(sprintf("Chi-square p-value: %.8f\n",
            dchisq(crs$glm$null.deviance-crs$glm$deviance,
                   crs$glm$df.null-crs$glm$df.residual)))
cat(sprintf("Pseudo R-Square (optimistic): %.8f\n",
             cor(crs$glm$y, crs$glm$fitted.values)))
cat('\n==== ANOVA ====\n\n')
print(anova(crs$glm, test="Chisq"))
cat("\n")

# Time taken: 8.22 hours

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2016-07-31 13:49:16 x86_64-w64-mingw32 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(Buy_or_not) ~ .,
    data=crs$dataset[crs$sample,c(crs$input, crs$target)],
    size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

#============================================================
# Rattle timestamp: 2016-07-31 13:57:27 x86_64-w64-mingw32 

# Evaluate model performance. 

# Generate an Error Matrix for the Ada Boost model.

# Obtain the response from the Ada Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

table(crs$dataset[crs$test, c(crs$input, crs$target)]$Buy_or_not, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(crs$dataset[crs$test, c(crs$input, crs$target)]$Buy_or_not, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

table(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Buy_or_not, crs$pr,
        useNA="ifany",
        dnn=c("Actual", "Predicted"))

# Generate the confusion matrix showing proportions.

pcme <- function(actual, cl)
{
  x <- table(actual, cl)
  nc <- nrow(x) # Number of classes.
  nv <- length(actual) - sum(is.na(actual) | is.na(cl)) # Number of values.
  tbl <- cbind(x/nv,
               Error=sapply(1:nc,
                 function(r) round(sum(x[r,-r])/sum(x[r,]), 2)))
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted")
  return(tbl)
}
per <- pcme(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Buy_or_not, crs$pr)
round(per, 2)

# Calculate the overall error percentage.

cat(100*round(1-sum(diag(per), na.rm=TRUE), 2))

# Calculate the averaged class error percentage.

cat(100*round(mean(per[,"Error"], na.rm=TRUE), 2))

#============================================================
# Rattle timestamp: 2016-07-31 14:12:00 x86_64-w64-mingw32 

# Score a dataset. 

#============================================================
# Rattle timestamp: 2016-07-31 14:12:14 x86_64-w64-mingw32 

# Score a dataset. 

#============================================================
# Rattle timestamp: 2016-07-31 14:13:35 x86_64-w64-mingw32 

# Score a dataset. 

# Obtain probability scores for the Ada Boost model on ds1.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- subset(crs$dataset[crs$test,], select=c("Buy_or_not"))

# Output the combined data.

write.csv(cbind(sdata, crs$pr), file="D:\Contests\ZS Associates\Models\ds1_test_score_idents.csv", row.names=FALSE)

#============================================================
# Rattle timestamp: 2016-07-31 14:18:32 x86_64-w64-mingw32 

# Score a dataset. 

# Read a dataset from file for testing the model.

crs$testset <- read.csv("D:/Contests/ZS Associates/OG/Solution.csv", na.strings=c(".", "NA", "", "?"), header=TRUE, sep=",", encoding="UTF-8", strip.white=TRUE)

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

#============================================================
# Rattle timestamp: 2016-07-31 14:19:25 x86_64-w64-mingw32 

# Score a dataset. 

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

#============================================================
# Rattle timestamp: 2016-07-31 14:20:23 x86_64-w64-mingw32 

# Score a dataset. 

# Ensure the levels are the same as the training data for variable `Hospital_ID'.

levels(crs$testset[["Hospital_ID"]]) <- 
  c(levels(crs$testset[["Hospital_ID"]]), 
    setdiff(levels(crs$dataset[["Hospital_ID"]]), 
               levels(crs$testset[["Hospital_ID"]])))

# Ensure the levels are the same as the training data for variable `District_ID'.

levels(crs$testset[["District_ID"]]) <- 
  c(levels(crs$testset[["District_ID"]]), 
    setdiff(levels(crs$dataset[["District_ID"]]), 
               levels(crs$testset[["District_ID"]])))

# Ensure the levels are the same as the training data for variable `Instrument_ID'.

levels(crs$testset[["Instrument_ID"]]) <- 
  c(levels(crs$testset[["Instrument_ID"]]), 
    setdiff(levels(crs$dataset[["Instrument_ID"]]), 
               levels(crs$testset[["Instrument_ID"]])))

#============================================================
# Rattle timestamp: 2016-07-31 20:37:15 x86_64-w64-mingw32 

# Reload the project data (variable crs) from file.

load("D:/Contests\ZS Associates\Models\ds1.rattle")



