##################
# Initialization #
##################
cat("\014"); rm(list = ls())
# install.packages("data.table")
# install.packages("tm")
# install.packages("FSelector")
# install.packages("caret")
# install.packages("ROCR")


################
# Get the data #
################
library(data.table)
reviews_neg <- fread("./data/Movie Review/rt-polarity.neg", sep="\n", data.table=FALSE, col.names="review", stringsAsFactors=FALSE)
reviews_pos <- fread("./data/Movie Review/rt-polarity.pos", sep="\n", data.table=FALSE, col.names="review", stringsAsFactors=FALSE)
reviews <- rbind(reviews_neg, reviews_pos)
labels  <- as.factor(c(rep("N",nrow(reviews_neg)), rep("P",nrow(reviews_pos))))


###############
# Text mining #
###############
library(tm)
## Set up a source for the text
reviews_source <- VectorSource(reviews[,])

## Create a corpus from that source 
corpus <- Corpus(reviews_source)

## Cleaning the text
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english")) # or use stopwords("SMART")
corpus <- tm_map(corpus, stemDocument) # requires the "SnowballC" package
#getTransformations() # display what the data cleaning options are
#stopwords("english") # what is getting removed

## Create the document-term matrix.
dtm <- DocumentTermMatrix(corpus)
dim(dtm)
dtm2 <- as.matrix(dtm)

## Create logical matrix
dtm_l    <- dtm2>0 #convert counts matrix to logical matrix
labels_l <- labels=="P"



##################
# Word Selection #
##################
dtm_l_df = as.data.frame(cbind(dtm_l, "labels"=labels_l))
set.seed(2016)
index_train = sample(1:nrow(reviews),round(0.7*nrow(reviews)))
p = 300

# Correlation
## Find the correlation between the independent variables and the dependent variable
W = cor(dtm_l, labels_l)
## Pick the top 'm' correlated independent variables
names(W) = colnames(dtm_l)
W = sort(abs(W), decreasing = TRUE)
index_col = c(colnames(dtm_l)[colnames(dtm_l) %in% names(W)[1:p]],"labels")


# Informaion Gain
# library("FSelector")
# start.time <- Sys.time()
# #for(chunk=)
# IG = information.gain(labels~., dtm_l_df)
# Sys.time() - start.time



#############################
# Fitting model on the data #
#############################
library(caret)
library(ROCR)
mdl = glm(labels~., dtm_l_df[index_train,index_col], family = "binomial")
summary(mdl)


#y_hat = predict(mdl, D[-index_train, ])
#caret::confusionMatrix(y_hat>0,D[-index_train,"labels"])
y_hat    = predict(mdl, dtm_l_df[-index_train,index_col], type="response")
pred     = ROCR::prediction(as.vector(y_hat),dtm_l_df[-index_train,"labels"])
perf_AUC = ROCR::performance(pred,"auc") #Calculate the AUC value
perf_AUC@y.values[[1]]
# p=40;  0.68
# p=100; 0.75
# p=400; 0.83


#######################
# Shuffle the dataset #
#######################
set.seed(2016)
dataset = dtm_l_df
# dataset = dataset[sample(nrow(dataset)),]


#######################
# Store the dataset #
#######################
file_name = paste0("dataset_v",p,".csv")
file_path = file.path(".","data","Movie Review",file_name)
file_name
write.csv(ifelse(dataset[,index_col]==FALSE,0,1), file_path, row.names=F)
