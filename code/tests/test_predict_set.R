################################################################################
## Unit testing for predict_set
################################################################################
## 1. Initialization
source("scripts/load_libraries.R")
sapply(list.files(pattern = "[.]R$", path = "./functions/", full.names = TRUE), source)
set.seed(2015)
## 2. Get the Data
library("kernlab")
data(spam)
dataset <- spam
## 3. Split the Data
index_train <- sample(nrow(dataset), round(nrow(dataset) * 0.7))
train_set <- dataset[index_train, ]
test_set <- dataset[-index_train, ]
################################################################################
## A. test that predict_set handles valid two input arguments
predict_set(train_set, test_set)

## B. test that predict_set handles the different inducers
results <- c()
for (inducer in c("RF", "GLM", "J48", "SVM")) {
    cat("\n Fit", inducer, "model")
    AUC <- predict_set(train_set, test_set, inducer = inducer)
    names(AUC) <- inducer
    results <- c(results, AUC)
}
results

## C. test that predict_set trows an error for unknown inducers
predict_set(train_set, test_set, inducer = "Non-Existing Inducer")
