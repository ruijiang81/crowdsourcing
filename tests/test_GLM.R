################################################################################
## Unit testing for GLM
################################################################################

####################
## Initialization ##
####################
seed_number = 2017
set.seed(seed_number)
#install.packages("e1071")
#library(e1071)
library(ROCR)


##################
## Get the Data ##
##################
library("kernlab")  
data(spam)
dataset = spam
colnames(dataset)[58] <- "y"


####################
## Split the Data ##
####################
index_train = sample(nrow(dataset),round(nrow(dataset)*0.7))
train_set = dataset[index_train,]
test_set  = dataset[-index_train,]


###################
## Fit GLM model ##
###################
set.seed(seed_number)
model = glm(y ~ ., data=train_set, family = "binomial")
str(model)
summary(model)


#######################
## Models evaluation ##
#######################
y_hat    = predict(model, test_set, type="response")
pred     = ROCR::prediction(as.vector(y_hat), test_set[,"y"])
perf_AUC = ROCR::performance(pred,"auc") #Calculate the AUC value
perf_AUC@y.values[[1]] #[1] 0.97 [2] 0.97 [3] 0.97

