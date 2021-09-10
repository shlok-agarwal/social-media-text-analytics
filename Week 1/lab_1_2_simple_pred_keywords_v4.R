rm(list=ls())

install.packages("readxl")
library(readxl)
library(DBI)
set.seed(100)

setwd("/Users/weiguang/Documents/CIS434/2021SpringB/labs/")

## load our data
drRatings <- read_excel("./data/OBGYN_new_train_80000.xlsx",n_max=10000)

## shuffle the data so that they are in random sequence
drRatings <- drRatings[sample(nrow(drRatings)),]

hist(drRatings$punctuality)
hist(drRatings$staff)
hist(drRatings$helpfulness)
hist(drRatings$knowledge)


# prepare input and output features
drRatings$highPunctuality <- as.numeric(drRatings$punctuality > 4)
summary(drRatings$highPunctuality)

drRatings$s<-factor(drRatings$state)

drRatings$Year <- format(drRatings$postedTime, format="%Y")
drRatings$y<-factor(drRatings$Year)


##logit model
model1 <- glm(highPunctuality~numReviews+s+y, family=binomial("logit"), data=drRatings)
summary(model1)

predict.glm(model1, drRatings[1:5,], type="response")

drRatings[1:5,]$highPunctuality



##predict
prediction1 <- predict.glm(model1, drRatings, type="response")
prediction1 = as.numeric(prediction1 > 0.5)
truth <- drRatings$highPunctuality

xtab <- table(prediction1, truth)
# load Caret package for computing Confusion matrix
install.packages("caret")
install.packages("stringr")
install.packages("e1071")
library(caret)
result1<-confusionMatrix(xtab)
result1$overall[['Accuracy']]


##probit model
model2 <- glm(highPunctuality~numReviews+s+y, family=binomial("probit"), data=drRatings)
summary(model2)

##predict
prediction2 <- predict.glm(model2, drRatings, type="response")
prediction2 = as.numeric(prediction2 > 0.5)
truth <- drRatings$highPunctuality
xtab <- table(prediction2, truth)
result2<-confusionMatrix(xtab)
result2$overall[['Accuracy']]


evaluate_acc <- function(model,drRatings){
  prediction <- predict.glm(model, drRatings, type="response")
  prediction = as.numeric(prediction > 0.5)
  truth <- drRatings$highPunctuality
  xtab <- table(prediction, truth)
  result<-confusionMatrix(xtab)
  return(result$overall[['Accuracy']])
}

evaluate_acc(model2, drRatings)


drRatings$good <- as.numeric(grepl("good", drRatings$review))
drRatings$bad <- as.numeric(grepl("bad", drRatings$review))
model3 <- glm(highPunctuality~good+bad+numReviews+s+y, family=binomial("logit"), data=drRatings)
# summary(model3)
evaluate_acc(model3, drRatings)


drRatings$time <- as.numeric(grepl("time", drRatings$review))
model4 <- glm(highPunctuality~time+good+bad+numReviews+s+y, family=binomial("logit"), data=drRatings)
# summary(model4)
evaluate_acc(model4, drRatings)


drRatings$wait <- as.numeric(grepl("wait", drRatings$review))
model5 <- glm(highPunctuality~wait+time+good+bad+numReviews+s+y, family=binomial("logit"), data=drRatings)
# summary(model5)
evaluate_acc(model5, drRatings)


drRatings$no <- as.numeric(grepl("no", drRatings$review))
model6 <- glm(highPunctuality~no+wait+time+good+bad+numReviews+s+y, family=binomial("logit"), data=drRatings)
# summary(model6)
evaluate_acc(model6, drRatings)




# drRatings$good <- as.numeric(grepl("good", drRatings$review))
# drRatings$bad <- as.numeric(grepl("bad", drRatings$review))
# drRatings$time <- as.numeric(grepl("time", drRatings$review))
# drRatings$wait <- as.numeric(grepl("wait", drRatings$review))
# drRatings$no <- as.numeric(grepl("no", drRatings$review))
# model3 <- glm(highPunctuality~no+wait+time+good+bad+numReviews+s+y, family=binomial("logit"), data=drRatings)
# summary(model3)

