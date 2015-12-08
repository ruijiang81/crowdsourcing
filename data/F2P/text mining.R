################################################################################
## Text mining in R
## https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
################################################################################
## Initialization
cat("\014"); rm(list = ls())


## Get the data
read.csv("./data/F2P/dataset.csv")

























################################################################################
## Developer Zone
################################################################################
# ## Bag of Words
# # install.packages("qdap")
# # library("qdap")
# # words <- all_words(dataset, apostrophe.remove=TRUE)
# # head(words)
# # bag_o_words(dataset, apostrophe.remove=TRUE)
# 
# 
# ## Create a term-document matrix from a corpus
# #install.packages(c("tm","bigmemory"))
# library("tm")
# library("bigmemory")
# corpus <- Corpus(VectorSource(dataset[1:10660,]))
# DTM <- DocumentTermMatrix(corpus,
#                           control = list())
# DTM
# 
# M=as.big.matrix(x=as.matrix(DTM))#convert the DTM into a bigmemory object using the bigmemory package 
# M=as.matrix(M)#convert the bigmemory object again to a regular matrix
# M_l = M>0 #convert counts matrix to logical matrix
# dim(M)
# 
# 
# ## Remove near zero variance predictors
# # library("caret")
# # p = 100
# # freq = t(round(prop.table(apply(M_l, 2, table),2),4))[,2]
# # freq = sort(freq, decreasing=TRUE)
# # hist(freq[1:100],20)
# # index_cols = colnames(M_l) %in% names((freq[1:p]))
# # M_nzr = nearZeroVar(M_l[,index_cols], freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE, 
# #                     foreach = FALSE, allowParallel = TRUE)
# # M_nzr
# #' freqRatio	    the ratio of frequencies for the most common value over the second most common value
# #' percentUnique	the percentage of unique data points out of the total number of data points
# #' zeroVar	        a vector of logicals for whether the predictor has only one distinct value
# #' nzv	            a vector of logicals for whether the predictor is a near zero variance predictor
# #' #
# # checkConditionalX(x=as.data.frame(M[,1:10]), y=labels)
# #
# # https://rpubs.com/nishantsbi/92510
# # sparse = tm::removeSparseTerms(frequencies, 0.995)
# 
# 
# ## Information gain
# # install.packages("FSelector")
# # library("FSelector")
# # D = as.data.frame(cbind(M_l[,index_col],labels))
# # IG = information.gain(labels~., D)
# # IG
# 
# set.seed(2016)
# index_train = sample(1:nrow(dataset),round(0.7*nrow(dataset)))
# p = 40
# 
# # Find the correlation between the independent variables and the dependent variable
# C = cor(M_l,labels=="P")
# # Pick the top 'm' correlated independent variables
# names(C) = colnames(M_l)
# sort(abs(C), decreasing = TRUE)[1:p]
# index_col = colnames(M_l)[colnames(M_l) %in% names(sort(abs(C), decreasing = TRUE)[1:p])]
# 
# D     = data.frame(M_l[,index_col],labels=(labels=="P"))
# mdl   = glm(labels~., D[index_train, ], family = "binomial")
# #y_hat = predict(mdl, D[-index_train, ])
# #caret::confusionMatrix(y_hat>0,D[-index_train,"labels"])
# y_hat    = predict(mdl, D[-index_train,], type="response")
# pred     = ROCR::prediction(as.vector(y_hat),D[-index_train,"labels"])
# perf_AUC = ROCR::performance(pred,"auc") #Calculate the AUC value
# perf_AUC@y.values[[1]]
# 
# 
# # p=40;  0.68/0.68
# # p=100; 0.75
# # p=400; 0.83
# 
# #summary(mdl)
# 
# # stopCluster(cl) 
# write.csv(ifelse(D==FALSE,0,1), "MR40_dataset.csv", row.names=F)
