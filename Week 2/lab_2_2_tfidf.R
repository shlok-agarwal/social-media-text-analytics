rm(list=ls())

library(readxl)
install.packages("tm") # use only run this for the 1st time 
library(tm)  
set.seed(100)

setwd("/Users/weiguang/Documents/CIS434/2021SpringB/labs/")

# 1. read data
drRatings <- read_excel("./data/OBGYN_new_train_80000.xlsx",n_max=100) # we use first 100 lines as an example

# 2. construct the corpus
drRatings_corpus = Corpus(VectorSource(drRatings$review))


# 3. DTM
## 3.1 DTM using raw text
dtm <- DocumentTermMatrix( drRatings_corpus )
inspect(dtm[1:10,1:8])

## 3.2 DTM using preprocessed text
dtm <- DocumentTermMatrix(drRatings_corpus, control=list(tolower=T, removePunctuation=T, 
                    removeNumbers=T, stripWhitespace=T, stopwords=stopwords("english")))
inspect(dtm[1:10,1:8])
dim(dtm)


# 4. tf.idf matrix
dtm.tfidf <- DocumentTermMatrix( drRatings_corpus, control=list(weighting=weightTfIdf,
                          tolower=T, removePunctuation=T,removeNumbers=T, 
                          stripWhitespace=T, stopwords=stopwords("english")) )
inspect(dtm.tfidf[1:10,1:8])
dim(dtm.tfidf)


# 5. keep only top terms as our vocabulary
dim(dtm.tfidf)  # check the number of unique terms -- which is the maximum size of our vocabulary
dtm.tfidf <- removeSparseTerms(dtm.tfidf,0.98)  # adjust this number 0.98 to any bigger or smaller number that gives you the best size of your vocabulary
dim(dtm.tfidf)  # check the size of your vocabulary


x <- as.matrix(dtm.tfidf)
y <- as.data.frame(x,names = dtm.tfidf$dimnames$Terms)








