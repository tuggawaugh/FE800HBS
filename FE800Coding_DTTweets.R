# Install Packages

# plotting and pipes - tidyverse!
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tidyr")
library(tidyr)
# text mining library
install.packages("tidytext")
library(tidytext)
install.packages("tm")
library(tm)
install.packages("NLP")
library(NLP)
# coupled words analysis
install.packages("widyr")
library(widyr)
# plotting packages
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)

### Create a Data Frame
getwd()
setwd("C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS")

## read Donald Trump tweets downloaded from trumptwitterarchive.com 
# realDT <- read.csv("DonaldTrumpTweets.csv",header = TRUE, stringsAsFactors=FALSE)
realDT <- readr::read_csv("dt_tweets.csv")

## see first 6 rows of the dataset
head(realDT)

tail(realDT)

## check number of rows and columns
nrow(realDT)
ncol(realDT)

## package
summary(realDT)

colnames(realDT)
head(realDT["text"])
head(realDT["created_at"])
is.factor(realDT["text"])
# is.String(realDT["text"])


## CREATE DATA FRAME WITH JUST DATE AND TWEET TEXT FOR MINING (need to fix time in Created_At column)
# DTTweets <- as.data.frame(realDT)
# DTTweets <- realDT[2:3]
DTTweets <- data.frame(date_time = realDT$created_at,
                         tweet_text = realDT$text)
# DTTweets <- data.frame(tweet_text = realDT$text)
is.factor(DTTweets["tweet_text"])
summary(DTTweets)
head(DTTweets)
class(DTTweets)
nrow(DTTweets)
ncol(DTTweets)
colnames(DTTweets)

mode(DTTweets$tweet_text)


## EXPLORE COMMON WORDS

tweet_messages <- DTTweets %>%
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text)

head(flood_tweet_messages)
##            word
## 1      download
## 1.1 centurylink
## 1.2   community
## 1.3       flood
## 1.4      impact
## 1.5      report

