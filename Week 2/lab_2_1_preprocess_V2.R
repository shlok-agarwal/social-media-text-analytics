rm(list=ls())
install.packages('tm')
library(tm)
install.packages('SnowballC')
setwd("/Users/weiguang/Documents/CIS434/2021SpringB/labs/")

# 1. create a dataframe with 5 raw tweets
t1 <- "COVID-19 (Coronavirus) Latest News & Statistics. We post updates 24/7 as they come in, all data is from https://t.co/XmYqcFcEtM - #Coronavirus #COVID19"
t2 <- "Still standing, but barely, while upholding your civil rights in Illinois against government corruption #Bitcoin #HEX crypto"
t3 <- "24:7 Worldwide News. Follow @iNewsroom for realtime reports & 
join the LIVE Newsroom at: https://t.co/MAlfMkJMNZ"
t4 <- '<a href="https://mobile.twitter.com" rel="nofollow">Twitter Web App</a>'
t5 <- 'Product design/product management for healthcare, enterprise and more. Author, Mastering Collaboration. Also I ?????? music. Persistent. She/her.'
data_tweet <- c(t1,t2,t3,t4,t5)
doc_id <- c(1,2,3,4,5)
data_tweet <- data.frame(doc_id = doc_id, text = data_tweet, stringsAsFactors = FALSE)


# 2. construct the corpus
# let R (tm package) know that we use this data as our corpus for text analysis
tweets_1 = Corpus(DataframeSource(data_tweet))
tweets_1[[1]]$content   #check the text of first document


# 3. noise removal -- for tweets
# 3.1 Retweet removal
removeRT <- function(x){gsub("(rt|via)((?:\\b\\W*@\\w+)+)", "", x)}
tweets_2 = tm_map(tweets_1,content_transformer(removeRT))
tweets_2[[1]]$content

# 3.2 Hashtag removal
removeHashtag <- function(x){gsub("#\\S+", "", x)}
tweets_3 = tm_map(tweets_2,content_transformer(removeHashtag))
tweets_3[[1]]$content

# 3.3 URL removal
removeURL <- function(x){gsub("http[^[:space:]]*", "", x)}
tweets_4 = tm_map(tweets_3,content_transformer(removeURL))
tweets_4[[1]]$content

# 3.4 HTML removal
unescapeHTML <- function(str) {return(gsub("<.*?>", "", str))}
tweets_5 = tm_map(tweets_4,content_transformer(unescapeHTML))
tweets_5[[1]]$content

# 3.5 Mention removal
removeMention <- function(x){gsub("@\\w+", "", x)}
tweets_6 = tm_map(tweets_5,content_transformer(removeMention))
tweets_6[[1]]$content

# 3.6 Carriage removal
removeCarriage <- function(x){gsub("[\r\n]", "", x)}
tweets_7 = tm_map(tweets_6,content_transformer(removeCarriage))
tweets_7[[1]]$content

# 3.7 Emoticon removal
removeEmoticon <- function(x){gsub("[^\x01-\x7F]", "", x)}
tweets_8 = tm_map(tweets_7,content_transformer(removeEmoticon))
tweets_8[[1]]$content



# 4. general preprocessing procedures
# 4.1. lowercase
tweets_9 = tm_map(tweets_8,content_transformer(tolower))
tweets_9[[1]]$content

# 4.2. removePunctuation
tweets_10 <- tm_map(tweets_9, removePunctuation)
tweets_10[[1]]$content

# 4.3. removeNumbers
tweets_11 <- tm_map(tweets_10,removeNumbers)
tweets_11[[1]]$content

# 4.4. remove stopwords
tweets_12 <- tm_map(tweets_11,removeWords,stopwords("english"))
tweets_12[[1]]$content

# 4.5. remove task specific stopwords
tweets_13 <- tm_map(tweets_12,removeWords,c("dr", "doctor"))
tweets_13[[1]]$content

# 4.6. stripWhitespace
tweets_14 <- tm_map(tweets_13,stripWhitespace)
tweets_14[[1]]$content

# 4.7. word stemming
tweets_15<-tm_map(tweets_14,stemDocument)
tweets_15[[1]]$content

#4.8. word lemmatization
install.packages('textstem')
library(textstem)
tweets_16 <- tm_map(tweets_15, lemmatize_strings)
tweets_16[[1]]$content
