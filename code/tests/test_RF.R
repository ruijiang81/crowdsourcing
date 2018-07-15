################################################################################
## Unit testing for SVM
################################################################################
cat("\014")
rm(list = ls())


####################
## Initialization ##
####################
seed_number <- 2016
set.seed(seed_number)
# install.packages("e1071")
library(e1071)


##################
## Get the Data ##
##################
library("kernlab")
data(spam)
dataset <- spam
colnames(dataset)[58] <- "y"


####################
## Split the Data ##
####################
index_train <- sample(nrow(dataset), round(nrow(dataset) * 0.7))
train_set <- dataset[index_train, ]
test_set <- dataset[-index_train, ]


##################
## Fit RF model ##
##################
set.seed(seed_number)
model <- randomForest::randomForest(y ~ ., train_set, ntree = 100)


#######################
## Models evaluation ##
#######################
y_hat <- predict(model, test_set, type = "prob")
y_hat <- y_hat[, 2]
### ROCR package
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"]) # , sort(levels(test_set[,"y"]), decreasing = T))
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
perf_AUC@y.values[[1]]
### AUC package
y_roc <- AUC::roc(y_hat, test_set[, "y"])
perf_AUC <- AUC::auc(y_roc)
perf_AUC
