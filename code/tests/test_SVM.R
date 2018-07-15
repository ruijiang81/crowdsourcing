################################################################################
## Unit testing for SVM
################################################################################

####################
## Initialization ##
####################
seed_number <- 2017
set.seed(seed_number)
# install.packages("e1071")
library(e1071)
library(ROCR)


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

####################
## Fit SVM models ##
####################
set.seed(seed_number)
svm_SFC1E0 <- svm(y ~ ., data = train_set, scale = F, cost = 1e0, probability = TRUE)
svm_STC1E0 <- svm(y ~ ., data = train_set, scale = T, cost = 1e0, probability = TRUE)
svm_SFC1E4 <- svm(y ~ ., data = train_set, scale = F, cost = 1e4, probability = TRUE)
svm_STC1E4 <- svm(y ~ ., data = train_set, scale = T, cost = 1e4, probability = TRUE)

#######################
## Models evaluation ##
#######################
results <- data.frame()
## svm_SFC1E0
y_hat <- predict(svm_SFC1E0, test_set, probability = TRUE)
y_hat <- attr(y_hat, "probabilities")[, 1]
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results["SFC1E0", "AUC"] <- perf_AUC@y.values[[1]] # [1] 0.90 [2] 0.90 [3] 0.09
## svm_STC1E0
y_hat <- predict(svm_STC1E0, test_set, probability = TRUE)
y_hat <- attr(y_hat, "probabilities")[, 1]
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results["STC1E0", "AUC"] <- perf_AUC@y.values[[1]] # [1] 0.97 [2] 0.97 [3] 0.03
## svm_SFC1E4
y_hat <- predict(svm_SFC1E4, test_set, probability = TRUE)
y_hat <- attr(y_hat, "probabilities")[, 1]
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results["SFC1E4", "AUC"] <- perf_AUC@y.values[[1]] # [1] 0.91 [2] 0.90 [3] 0.09
## svm_STC1E4
y_hat <- predict(svm_STC1E4, test_set, probability = TRUE)
y_hat <- attr(y_hat, "probabilities")[, 1]
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results["STC1E4", "AUC"] <- perf_AUC@y.values[[1]] # [1] 0.94 [2] 0.94 [3] 0.06

results
