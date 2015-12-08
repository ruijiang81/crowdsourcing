################################################################################
## Text mining in R
## https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
################################################################################
## Initialization
cat("\014"); rm(list = ls())


## Get the data
reviews <- read.csv("./data/F2P/dataset.csv", stringsAsFactors=FALSE)
str(reviews) #have a quick look at reviews to see if the csv has loaded correctly.


################################################################################
## Using the tm package - the text mining library for R.
################################################################################
#' First install and load the 'tm' library into your session, 
#' 
#install.packages('tm')
library(tm)
#' The tm package is designed for comparing different texts against each other. 
#' These are the steps the tm package expects you to take:
#' 
#' 1. Set up a source for your text
#' 2. Create a corpus from that source (a corpus is just another name for a collection of texts)
#' 3. Create a document-term matrix, which tells you how frequently each term appears in each document in your corpus
#' 
#' We currently have all the text of every review in a vector, reviews$text, of 
#' size 1000. Each element of the vector corresponds to one review. Since we’re
#' currently not interested in the difference between each review, we can simply
#' paste every review together, separating with a space.
#' 
review_text <- paste(reviews$text, collapse=" ")
#' The collapse argument in paste tells R that we want to paste together 
#' elements of a vector, rather than pasting vectors together.
#' Now we can set up the source and create a corpus.
#' 
review_source <- VectorSource(review_text)
corpus        <- Corpus(review_source)
#' Easy!
#' Next, we begin cleaning the text. We use the multipurpose tm_map function 
#' inside tm to do a variety of cleaning tasks:
#' 
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english")) # or use stopwords("SMART")
#' So, what have we just done? 
#' 
#' 1. We’ve transformed every word to lower case, so that ‘Fun’ and ‘fun’ now count as the same word. 
#' 2. We’ve removed all punctuation – ‘fun’ and ‘fun!’ will now be the same. 
#' 3. We stripped out any extra whitespace. 
#' 4. we removed stop words. Stop words are just common words which we may not be interested in. 
#' 
#' If we look at the result of stopwords ("english") we can see what is getting removed:
#' 
stopwords("english")
#' Depending out what you are trying to achieve with your analysis, you may want 
#' to do the data cleaning step differently. You may want to know what 
#' punctuation is being used in your text or the stop words might be an 
#' important part of your analysis. So use your head and have a look at the 
#' getTransformations() function to see what your data cleaning options are.
#' 
getTransformations()
#' Now we create the document-term matrix.
#' 
dtm <- DocumentTermMatrix(corpus)
#' Since we only have one document in this case, our document-term matrix will 
#' only have one column.
#' The tm package stores document term matrixes as sparse matrices for efficacy. 
#' Since we only have 1000 reviews and one document we can just convert our 
#' term-document-matrix into a normal matrix, which is easier to work with.
#' 
dtm2 <- as.matrix(dtm)
#' We then take the column sums of this matrix, which will give us a named 
#' vector.
#' 
frequency <- colSums(dtm2)
#' And now we can sort this vector to see the most frequently used words:
#' 
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)
#' Voila!
    
    
################################################################################
## Plotting a word cloud
################################################################################
#' However, a list of words and frequencies is a little hard to interpret. Let’s 
#' install and load the wordcloud package to visualize these words as a word 
#' cloud.
#' 
#install.packages('wordcloud')
library(wordcloud)
#' Word cloud is very easy to use, we just need a list of the words to be 
#' plotted and their frequency. To get the list of words we can take the names 
#' of our named vector.
#' 
words <- names(frequency)
#' Let’s plot the top 100 words in our cloud.
#' 
set.seed(2016)
wordcloud(words[1:100], frequency[1:100])

