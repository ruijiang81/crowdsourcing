## Initialization
rm(list = ls())
source("scripts/load_libraries.R")
sapply(list.files(pattern="[.]R$", path="./functions/", full.names=TRUE), source)
# options(error=recover) # debugging mode
options(digits=4)

library("kernlab")  
data(spam)
dataset <- spam

dataset <- setVariablesNames(dataset)

set.seed(2016)
index_holdout  = sample(nrow(dataset), round(nrow(dataset)*0.3))
holdout_data   = dataset[index_holdout,]
unlabeled_data = dataset[-index_holdout,]
unlabeled_data = unlabeled_data[sample(nrow(unlabeled_data)),]
max_size_training♦_data = nrow(unlabeled_data) #used later for sanity check


##########
# adabag #
##########
library(adabag)
set.seed(2016)
model_adabag = adabag::bagging(y ~ ., unlabeled_data, mfinal = 100)
y_hat1 = predict(model_adabag, holdout_data)$prob[,2]


library(ipred)
set.seed(2016)
model_ipred = ipred::bagging(y ~ ., unlabeled_data)
y_hat2 = predict(model_ipred, holdout_data, type="prob")[,"level2"]

par(pty="s")
plot(y_hat1, y_hat2, type="p",col=as.numeric(holdout_data$y), 
     xlab="adabag", ylab="ipred")
abline(v=0.5,h=0.5)
