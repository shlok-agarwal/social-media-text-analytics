rm(list=ls())

library(readxl)
library(tm)  
library(DBI)
set.seed(100)

setwd("/Users/weiguang/Documents/CIS434/2021SpringB/labs/")

# 1. read and prepare data 
drRatings <- read_excel("./data/OBGYN_new_train_80000.xlsx")
drRatings_test <- read_excel("./data/OBGYN_new_test_lab_withoutAnswer_100.xlsx")

n_record_train <- dim(drRatings)[1]
n_record_test <- dim(drRatings_test)[1]

drRatings$highPunctuality <- as.numeric(drRatings$punctuality > 4)
drRatings$s<-factor(drRatings$state)
drRatings$Year <- format(drRatings$postedTime, format="%Y")
drRatings$y<-factor(drRatings$Year)

drRatings_test$s<-factor(drRatings_test$state)
drRatings_test$Year <- format(drRatings_test$postedTime, format="%Y")
drRatings_test$y<-factor(drRatings_test$Year)

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


# 2. TFIDF matrix
drRatings_corpus = Corpus(VectorSource(c(drRatings$review,drRatings_test$review)))
dtm.tfidf <- DocumentTermMatrix( drRatings_corpus, control=list(weighting=weightTfIdf,
                          tolower=T, removePunctuation=T,removeNumbers=T, 
                          stripWhitespace=T, stopwords=c(stopwords("english"),'dr','doctor')))
dtm.tfidf <- removeSparseTerms(dtm.tfidf,0.98)
dim(dtm.tfidf)


# 3. construct the training data and train logit model
x <- as.matrix(dtm.tfidf)
y <- as.data.frame(x[1:n_record_train,1:dim(x)[2]],names = dtm.tfidf$dimnames$Terms)
newdf <- cbind(drRatings[c("highPunctuality","s","y")], y)
model <- glm(highPunctuality~., family=binomial("logit"), data=newdf)
evaluate_acc(model, newdf)


# 4. out-of-sample prediction
y <- as.data.frame(x[n_record_train+1:n_record_test,1:dim(x)[2]],names = dtm.tfidf$dimnames$Terms)
newdf_test <- cbind(drRatings_test[c("s","y")], y)
prediction <- predict.glm(model, newdf_test, type="response")
prediction = as.numeric(prediction > 0.5)
newdf_test_out <- cbind(drRatings_test[c("reviewID")], prediction)
write.csv(newdf_test_out, file="myPred.csv", row.names=FALSE)





######################################################################################
#### Below is how your HW1 will be evaluated, this is not a part of the lab ##########
######################################################################################

drRatings_test_evaluate <- read_excel("./data/OBGYN_new_test_lab_withAnswer_100.xlsx")
drRatings_test_pred <- read.csv("myPred.csv")
drRatings_test_evaluate$highPunctuality <- as.numeric(drRatings_test_evaluate$punctuality > 4)
truth <- drRatings_test_evaluate$highPunctuality
xtab <- table(prediction, truth)
result<-confusionMatrix(xtab)
result$overall[['Accuracy']]










