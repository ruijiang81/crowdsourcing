################################################################################
## Unit testing for GLM
################################################################################

####################
## Initialization ##
####################
seed_number <- 2017
set.seed(seed_number)
source("./functions/utilities.R")
library(ROCR)


##################
## Get the Data ##
##################
## Spam
# library("kernlab")
# data(spam)
# dataset = spam
## Movies Reviews
source("./data/Movie Review/import dataset.R")
dataset <- setVariablesNames(dataset)


####################
## Split the Data ##
####################
n <- nrow(dataset)
p <- ncol(dataset) - 1
index_train <- sample(n, 100) # round(n*0.7))
train_set <- dataset[index_train, ]
test_set <- dataset[-index_train, ]
table(train_set[, "y"])


#########################
## Data Pre-Processing ##
#########################
## Check which variables in the train-set have less then two levels, that is ZV
## (Zero Variance)
zv_var <- apply(train_set, 2, function(x) length(unique(x)) < 2)
if (sum(zv_var) > 0) {
    cat("\n Removed", sum(zv_var), "independent variables with zero variance")
    train_set <- train_set[, !zv_var]
}


###################
## Fit GLM model ##
###################
set.seed(seed_number)
model <- glm(y ~ ., data = train_set, family = "binomial")
# str(model)
# summary(model)


#######################
## Models evaluation ##
#######################
results <- c(NA, NA)
names(results) <- c("In-Sample-AUC", "Out-Sample-AUC")

y_hat <- predict(model, train_set, type = "response")
pred <- ROCR::prediction(as.vector(y_hat), train_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results[1] <- perf_AUC@y.values[[1]]

y_hat <- predict(model, test_set, type = "response")
pred <- ROCR::prediction(as.vector(y_hat), test_set[, "y"])
perf_AUC <- ROCR::performance(pred, "auc") # Calculate the AUC value
results[2] <- perf_AUC@y.values[[1]]

results
