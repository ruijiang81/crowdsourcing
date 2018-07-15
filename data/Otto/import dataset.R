dataset <- read.csv(file.path(getwd(), "data", "Otto", "train.csv"), colClasses = c("integer", rep("numeric", 93), "factor"))
dataset <- dataset[, -1] # deletes the first (ID) column
dataset <- oneVsAll(dataset, positive.class = 3) # customized function that assigns one positive and one negative class
