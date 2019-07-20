# Install Packages

# plotting and pipes - tidyverse!
install.packages("readr")
library(readr)
install.packages("ggplot2")
library(ggplot2)
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
# last package
install.packages("dplyr")
library(dplyr)
tidytext::unnest_tokens()


### Create a Data Frame
getwd()
#setwd("C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS")
#setwd("C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS")
setwd("C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS")

## read Donald Trump tweets downloaded from trumptwitterarchive.com 
realDT <- read.csv("dt_tweets.csv",header = TRUE, stringsAsFactors=FALSE)
#realDT <- readr::read_csv("dt_tweets.csv")


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

mode(DTTweets$date_time)
mode(DTTweets$tweet_text)
is.numeric(DTTweets$tweet_text)
DTTweets$tweet_text <- as.character(DTTweets$tweet_text)
is.character(DTTweets$tweet_text)


## EXPLORE COMMON WORDS

tweet_messages <- DTTweets %>%
  dplyr::select(tweet_text) %>%
  tidytext::unnest_tokens(word, tweet_text)

head(tweet_messages)

# plot the top 25 words
tweet_messages %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

## REMOVE STOP WORDS
data("stop_words")
# how many words do you have including the stop words?
nrow(tweet_messages)
## [1] 205078
## [1] 208009 bp

tweet_messages_clean <- tweet_messages %>%
  anti_join(stop_words) %>%
  filter(!word == "rt")
## Joining, by = "word"

# how many words after removing the stop words?
nrow(tweet_messages_clean)
## [1] 97409
## [1] 99906 bp


# plot the top 25 words again after removing stop words
tweet_messages_clean %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")


# Remove URLs
# cleanup
tweet_messages_clean <- DTTweets %>%
  mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                           "", tweet_text)) %>% 
  dplyr::select(tweet_text) %>%
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt") # remove all rows that contain "rt" or retweet
## Joining, by = "word"

# plot the top 25 words again after removing stop words AND URLs
tweet_messages_clean %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")

### PAIRED WORD Analysis
# workaround install package tm

tweets_paired <- DTTweets %>%
  dplyr::select(tweet_text) %>%
  mutate(tweet_text = removeWords(tweet_text, stop_words$word)) %>%
  mutate(tweet_text = gsub("\\brt\\b|\\bRT\\b", "", tweet_text)) %>%
  mutate(tweet_text = gsub("http://*", "", tweet_text)) %>%
  unnest_tokens(paired_words, tweet_text, token = "ngrams", n = 2)

tweets_paired %>%
  count(paired_words, sort = TRUE)

## Separate Words into Columns
tweets_separated <- tweets_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

# new bigram counts:
word_counts <- tweets_separated %>%
  count(word1, word2, sort = TRUE)
word_counts
# word_counts[1:50,]

# plot word network 
# fixed by manually installing igraph,ggraph then running code
word_counts %>%
  filter(n >= 30) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Donald Trump Tweets",
       subtitle = "Text mining twitter data ",
       x = "", y = "") +
  theme_void()


#Sentiment Analysis
library(tidytext)
install.packages("textdata")

sentiments

#afinn lexicon assigns words with a score that runs between -5 and 5 (positive score, positive sentiment)
get_sentiments("afinn")

# bing lexicon categorizes words in a binary fashion into positive and negative categories
get_sentiments("bing")

get_sentiments("loughran")


#############################NRC lexicon is no longer available within tidytext
## nrc categorizes words in a binary fashion ("yes"/"no") into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.
## NRC lexicon is no longer available within tidytext as per https://github.com/juliasilge/tidytext/issues/144
#get_sentiments("nrc")
#install.packages("syuzhet")
#library(syuzhet)

# Extract only the text column from the DDTweet dataframe
#tweet_text_only <- DTTweets$tweet_text
#head(tweet_text_only)

## Extracts the NRC sentiment scores for each tweet
#nrc_data <- get_nrc_sentiment(tweet_text_only)

