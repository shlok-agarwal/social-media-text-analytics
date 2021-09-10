rm(list=ls())

library(readxl)
library(tm)  
library(DBI)
set.seed(100)

setwd("/Users/weiguang/Documents/CIS434/2021SpringB/labs/")

#1. read and prepare data
drRatings <- read_excel("./data/OBGYN_new_train_80000.xlsx")
drRatings$highPunctuality <- as.numeric(drRatings$punctuality > 4)
drRatings$s<-factor(drRatings$state)
drRatings$Year <- format(drRatings$postedTime, format="%Y")
drRatings$y<-factor(drRatings$Year)
## evaluation function -- accuracy
library(caret)
evaluate_acc <- function(model,drRatings){
  prediction <- predict.glm(model, drRatings, type="response")
  prediction = as.numeric(prediction > 0.5)
  truth <- drRatings$highPunctuality
  xtab <- table(prediction, truth)
  result<-confusionMatrix(xtab)
  return(result$overall[['Accuracy']])
}


######################################################################################
#############################       Lab 1       ######################################
######################################################################################

# 2. replicate keyword searching model in Lab 1. 
drRatings$good <- as.numeric(grepl("good", drRatings$review))
drRatings$bad <- as.numeric(grepl("bad", drRatings$review))
drRatings$time <- as.numeric(grepl("time", drRatings$review))
drRatings$wait <- as.numeric(grepl("wait", drRatings$review))
drRatings$no <- as.numeric(grepl("no", drRatings$review))
model1 <- glm(highPunctuality~no+wait+time+good+bad+numReviews+s+y, 
              family=binomial("logit"), data=drRatings)
# summary(model1)

evaluate_acc(model1, drRatings)

######################################################################################
######################################################################################
#############################       Lab 2       ######################################
######################################################################################

# 3. TFIDF matrix
drRatings_corpus = Corpus(VectorSource(drRatings$review))
dtm.tfidf <- DocumentTermMatrix( drRatings_corpus, control=list(weighting=weightTfIdf,
                                tolower=T, removePunctuation=T,removeNumbers=T, 
                                stripWhitespace=T, stopwords=stopwords("english")) )
dtm.tfidf <- removeSparseTerms(dtm.tfidf,0.98)
dim(dtm.tfidf)


# 4. merge TFIDF with original data
x <- as.matrix(dtm.tfidf)
y <- as.data.frame(x,names = dtm.tfidf$dimnames$Terms)
newdf <- cbind(drRatings[c("highPunctuality","s","y")], y)
dim(newdf)


# 5. in-sample prediction
model2 <- glm(highPunctuality~., family=binomial("logit"), data=newdf)
# summary(model2)
evaluate_acc(model2, newdf)











