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
install.packages("textdata")
library(textdata)
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
# tidytext::unnest_tokens()
install.packages("wordcloud")
library(wordcloud)



### Create a Data Frame for DT Ttweets
getwd()
#setwd("C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS")
#setwd("C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS")
setwd("C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS")

## read Donald Trump tweets downloaded from trumptwitterarchive.com 
DTTweets_CSV <- read.csv("dt_tweets.csv",header = TRUE, stringsAsFactors=FALSE)


## see first and last rows of the dataset
head(DTTweets_CSV)
tail(DTTweets_CSV)

## check number of rows and columns
nrow(DTTweets_CSV)
ncol(DTTweets_CSV)

## review columns and rename as needed
summary(DTTweets_CSV)
colnames(DTTweets_CSV)
names(DTTweets_CSV)[names(DTTweets_CSV) == "ï..minuteid"] <- "minuteID"
colnames(DTTweets_CSV)
mode(DTTweets_CSV$date)

## Set the column width to 2 (and pad 0 upfront if single digit)
DTTweets_CSV$month <- formatC(DTTweets_CSV$month, flag=0, width=2)
DTTweets_CSV$date <- formatC(DTTweets_CSV$date, flag=0, width=2)
DTTweets_CSV$hour <- formatC(DTTweets_CSV$hour, flag=0, width=2)
DTTweets_CSV$minute <- formatC(DTTweets_CSV$minute, flag=0, width=2)
DTTweets_CSV$second <- formatC(DTTweets_CSV$second, flag=0, width=2)

## Create TimeID variable up to Minute to uniquely identify each tweet
DTTweets_CSV <- transform(DTTweets_CSV,timeID=paste0(year,month,date,hour,minute))
colnames(DTTweets_CSV)
head(DTTweets_CSV["timeID"])
DTTweets_CSV$timeID <- as.numeric(DTTweets_CSV$timeID)
mode(DTTweets_CSV$timeID)

## Export CSV
write.csv(DTTweets_CSV,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_ID.csv', row.names = FALSE)

## Create a data frame with just Core data - identifiers and tweet text
DTTweets_Core <- data.frame(time_id = DTTweets_CSV$timeID,
                       date_time = DTTweets_CSV$created_at,
                         tweet_text = DTTweets_CSV$text)
is.factor(DTTweets_Core["tweet_text"])
summary(DTTweets_Core)
head(DTTweets_Core)
colnames(DTTweets_Core)

## Validate the mode / class
mode(DTTweets_Core$time_id)
mode(DTTweets_Core$date_time)
mode(DTTweets_Core$tweet_text)
is.numeric(DTTweets_Core$tweet_text)
DTTweets_Core$tweet_text <- as.character(DTTweets_Core$tweet_text)
is.character(DTTweets_Core$tweet_text)

# Assign a numerical indicator to each tweet
DTTweets_Core$tweet_id <- as.numeric(rownames(DTTweets_Core))
DTTweets_Core[1:30,c("tweet_id","time_id")]
colnames(DTTweets_Core)

## EXPLORE COMMON WORDS

DTTweets_Words <- DTTweets_Core %>%
  dplyr::select(tweet_text, tweet_id, time_id, date_time) %>%
  tidytext::unnest_tokens(word, tweet_text)

head(DTTweets_Words)


## plot the top 25 words
DTTweets_Words %>%
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
nrow(DTTweets_Words)
## [1] 208009

DTTweets_Words_clean <- DTTweets_Words %>%
  anti_join(stop_words) %>%
  filter(!word == "rt")

# how many words after removing the stop words?
nrow(DTTweets_Words_clean)
## [1] 99906


# plot the top 25 words again after removing stop words
DTTweets_Words_clean %>%
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
DTTweets_Words_clean <- DTTweets_Core %>%
  mutate(tweet_text = gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", 
                           "", tweet_text)) %>% 
  dplyr::select(tweet_text, tweet_id, time_id, date_time) %>%
  unnest_tokens(word, tweet_text) %>% 
  anti_join(stop_words) %>%
  filter(!word == "rt") # remove all rows that contain "rt" or retweet
## Joining, by = "word"

# plot the top 25 words again after removing stop words AND URLs
DTTweets_Words_clean %>%
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

mode(DTTweets_Words_clean)

# ### PAIRED WORD Analysis
# # workaround install package tm
# 
# DTTweets_Pair <- DTTweets_Core %>%
#   dplyr::select(tweet_text, tweet_id, time_id, date_time) %>%
#   mutate(tweet_text = removeWords(tweet_text, stop_words$word)) %>%
#   mutate(tweet_text = gsub("\\brt\\b|\\bRT\\b", "", tweet_text)) %>%
#   mutate(tweet_text = gsub("http://*", "", tweet_text)) %>%
#   unnest_tokens(paired_words, tweet_text, token = "ngrams", n = 2)
# 
# DTTweets_Pair %>%
#   count(paired_words, sort = TRUE)
# 
# ## Separate Words into Columns
# DTTweets_PairSeparated <- DTTweets_Pair %>%
#   separate(paired_words, c("word1", "word2"), sep = " ")
# 
# # new bigram counts:
# word_counts <- DTTweets_PairSeparated %>%
#   count(word1, word2, sort = TRUE)
# word_counts
# # word_counts[1:50,]
# 
# # plot word network 
# # fixed by manually installing igraph,ggraph then running code
# word_counts %>%
#   filter(n >= 30) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
#   geom_node_point(color = "darkslategray4", size = 3) +
#   geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
#   labs(title = "Word Network: Donald Trump Tweets",
#        subtitle = "Text mining twitter data ",
#        x = "", y = "") +
#   theme_void()


### Sentiment Analysis

sentiments

## Bing lexicon - categorizes words in a binary fashion into positive and negative categories

# get_sentiments("bing")
# get_sentiments("bing") %>% 
#   count(sentiment)
# 
# bing_word_counts <- DTTweets_Words_clean %>%
#   inner_join(get_sentiments("bing")) %>%
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup()
# 
# bing_word_counts
# 
# bing_word_counts %>%
#   group_by(sentiment) %>%
#   top_n(10) %>%
#   ungroup() %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n, fill = sentiment)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~sentiment, scales = "free_y") +
#   labs(y = "Contribution to sentiment",
#        x = NULL) +
#   coord_flip()
# 
# DTTweets_Words_clean %>%
#   anti_join(stop_words) %>%
#   count(word) %>%
#   with(wordcloud(word, n, max.words = 100))


## Afinn Lexicon - assigns words with a score that runs between -5 and 5 (positive score, positive sentiment)
get_sentiments("afinn")

get_sentiments("afinn") %>% 
  count(value)

# Calculate the count of words with sentiment score
afinn_word_counts <- DTTweets_Words_clean %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

afinn_word_counts
mode(afinn_word_counts$value)

# Assign sentiment score to each word
afinn_word_value <- DTTweets_Words_clean %>%
  inner_join(get_sentiments("afinn"))


afinn_word_value
colnames(afinn_word_value)
mode(afinn_word_value$time_id)
head(afinn_word_value$time_id)
afinn_word_value[1:10,]

# Consolidate Sentiment values by Tweet using aggregate() function
DTTweets_AfinnValues <- aggregate(value ~ time_id, afinn_word_value, sum)
head(DTTweets_AfinnValues)
DTTweets_AfinnValues[order(DTTweets_AfinnValues$tweet_id),]
DTTweets_AfinnValues[1:10,]
# head(aggregate(value ~ ., afinn_word_value, sum))

# Consolidate Sentiment values by Tweet using group_by() function and pipes in dplyr
DTTweets_Afinn <-  afinn_word_value %>%
  group_by(time_id) %>% 
  summarise(net_sentiment = sum(value))
DTTweets_Afinn
DTTweets_Afinn[1:10,]
nrow(DTTweets_Afinn)
# THIS IS THE DATA SET WHICH NET SENTIMENT SCORE FOR EACH TWEET (TIME_ID)
# Export the data set with Net Sentiment Score
write.csv(DTTweets_Afinn,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_Afinn.csv', row.names = FALSE)

# Analyze the Histogram - Spread of net sentiment score for 5741 tweets
summary(DTTweets_Afinn)
hist(DTTweets_Afinn$net_sentiment)

DTTweets_AfinnCount <- DTTweets_Afinn %>% 
  count(net_sentiment)
DTTweets_AfinnCount

