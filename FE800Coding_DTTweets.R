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
install.packages("NLP")
library(NLP)
install.packages("tm")
library(tm)
install.packages("textdata")
library(textdata)
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
setwd("C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS")
#setwd("C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS")

## read Donald Trump tweets downloaded from trumptwitterarchive.com 
DTTweets_CSV <- read.csv("dt_tweets.csv",header = TRUE, stringsAsFactors=FALSE)


## see first and last rows of the dataset
head(DTTweets_CSV)
tail(DTTweets_CSV)

## check number of rows and columns (does not include retweets)
nrow(DTTweets_CSV)
# [1] 7151
ncol(DTTweets_CSV)
# [1] 16

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
mode(DTTweets_CSV$timeID)

## Export CSV
#write.csv(DTTweets_CSV,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_ID.csv', row.names = FALSE)
write.csv(DTTweets_CSV,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_ID.csv', row.names = FALSE)


## Create a data frame with just Core data - identifiers and tweet text
DTTweets_Core <- data.frame(time_id = DTTweets_CSV$timeID,
                       date_time = DTTweets_CSV$created_at,
                         tweet_text = DTTweets_CSV$text)
is.factor(DTTweets_Core["tweet_text"])
summary(DTTweets_Core)
head(DTTweets_Core)
nrow(DTTweets_Core)
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


# create a df where keywords are mentioned in a tweet. ensure all were in lowercase format.

keywords <- as.character(c("tax cut", "stock market","china","trade","tariff"))

head(keywords)
summary(keywords)

DTTweets_Core <- DTTweets_Core[grepl(paste(keywords, collapse="|"), DTTweets_Core$tweet_text),]

head(DTTweets_Core)
nrow(DTTweets_Core)

write.csv(DTTweets_Core,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_Core.csv', row.names = FALSE)


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
nrow(stop_words)
# how many words do you have including the stop words?
nrow(DTTweets_Words)
## [1] 190501

DTTweets_Words_clean <- DTTweets_Words %>%
  anti_join(stop_words) %>%
  filter(!word == "rt")

# how many words after removing the stop words?
nrow(DTTweets_Words_clean)
## [1] 79035


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

# DTTweets_Words_clean$word <- DTTweets_Words_clean$word[DTTweets_Words_clean$word!= "amp"]
DTTweets_Words_clean <- DTTweets_Words_clean[DTTweets_Words_clean$word!= "amp", ]
DTTweets_Words_clean <- DTTweets_Words_clean[DTTweets_Words_clean$word!= "â", ]
DTTweets_Words_clean <- DTTweets_Words_clean[DTTweets_Words_clean$word!= "trump", ]
DTTweets_Words_clean

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

get_sentiments("bing")
get_sentiments("bing") %>%
  count(sentiment)

# Cross each word with Bing library and validate if the Sentiment has been assigned correctly
bing_word_counts <- DTTweets_Words_clean %>%
  inner_join(get_sentiments("bing"))
bing_word_counts

# Count the most common words with negative and positive Sentiment 
bing_word_counts <- bing_word_counts %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts

# Plot the top 10 positive and negative words
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Create a word cloud of the most common 100 words
DTTweets_Words_clean %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

bing_word_counts

# Map the sentiment to each word
bing_word_sentiment <- DTTweets_Words_clean %>%
  inner_join(get_sentiments("bing"))
bing_word_sentiment

colnames(bing_word_sentiment)
mode(bing_word_sentiment$time_id)
head(bing_word_sentiment$time_id)
bing_word_sentiment[1:10,]

# Consolidate Sentiment values by Tweet using aggregate() function
DTTweets_BingBySentiment <- bing_word_sentiment %>%
  group_by(time_id) %>%
  count(sentiment)
DTTweets_BingBySentiment
DTTweets_BingBySentiment[1:20,]
mode(bing_word_sentiment$sentiment)

# COnvert all counts for Negative sentiment to negative values by multiplying by -1
DTTweets_BingBySentiment$n[DTTweets_BingBySentiment$sentiment == "negative"] <- DTTweets_BingBySentiment$n[DTTweets_BingBySentiment$sentiment == "negative"] * (-1)
DTTweets_BingBySentiment[1:20,]

# Rename the column from 'n' to 'count'
names(DTTweets_BingBySentiment)[names(DTTweets_BingBySentiment) == "n"] <- "count"

# Add the counts of Positive and Negative words by Tweet
DTTweets_BingByTweet <-  DTTweets_BingBySentiment %>%
  group_by(time_id) %>% 
  summarise(net_sentiment = sum(count))
DTTweets_BingByTweet
DTTweets_BingByTweet[1:20,]
nrow(DTTweets_BingByTweet)

# Confirm all Time_ID values are unique
DTTweets_Bing <-  DTTweets_BingByTweet %>%
  group_by(time_id)

# Convert to data frame
DTTweets_Bing <- as.data.frame(DTTweets_Bing)
DTTweets_Bing
nrow(DTTweets_Bing)

# THIS IS THE DATA SET WITH NET SENTIMENT SCORE - USING BING - FOR EACH TWEET (TIME_ID)
# THIS SHOULD FEED INTO STOCK ANALYSIS

# Export the data set with Net Sentiment Score
write.csv(DTTweets_Bing,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_Bing.csv', row.names = FALSE)
# write.csv(DTTweets_Bing,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_Bing.csv', row.names = FALSE)

# Analyze the Histogram - Spread of net sentiment score for 5128 tweets
summary(DTTweets_Bing)
hist(DTTweets_Bing$net_sentiment)
nrow(DTTweets_Bing)

# rm(DTTweets_BingIndexPositive)
# DTTweets_BingIndexPositive <- rownames(DTTweets_Bing$net_sentiment[DTTweets_Bing$net_sentiment > 0])
# as.numeric(rownames(DTTweets_Bing$net_sentiment[DTTweets_Bing$net_sentiment > 0]))
# as.numeric(rownames(DTTweets_Bing))


# Filter Tweets with positive values
DTTweets_BingPositive <- DTTweets_Bing %>%
  filter(net_sentiment > 0) # only select rows with net POSITIVE sentiment score 
DTTweets_BingNegative <- DTTweets_Bing %>%
  filter(net_sentiment < 0) # only select rows with net NEGATIVE sentiment score
nrow(DTTweets_BingPositive)
nrow(DTTweets_BingNegative)
DTTweets_BingNegative5 <- DTTweets_Bing %>%
  filter(net_sentiment <= -5) # only select rows with net NEGATIVE sentiment score of 5 or lower

# Review the Text of Negative Tweets 
(DTTweets_Core$tweet_text[DTTweets_BingNegative$time_id])[1:20]

# Review the Text of Tweets with Negative score of -5 or lower 
(DTTweets_Core$tweet_text[DTTweets_BingNegative5$time_id])[1:20]



### AFINN Lexicon - assigns words with a score that runs between -5 and 5 (positive score, positive sentiment)
get_sentiments("afinn")
# 2477 rows

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
DTTweets_AfinnValues[order(DTTweets_AfinnValues$time_id),]
DTTweets_AfinnValues[1:10,]
# head(aggregate(value ~ ., afinn_word_value, sum))

# Consolidate Sentiment values by Tweet using group_by() function and pipes in dplyr
DTTweets_Afinn <-  afinn_word_value %>%
  group_by(time_id) %>% 
  summarise(net_sentiment = sum(value))
DTTweets_Afinn
DTTweets_Afinn[1:20,]
nrow(DTTweets_Afinn)
# THIS IS THE DATA SET WITH NET SENTIMENT SCORE - USING AFINN - FOR EACH TWEET (TIME_ID)
# THIS SHOULD FEED INTO STOCK ANALYSIS

# Export the data set with Net Sentiment Score
#write.csv(DTTweets_Afinn,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_Afinn.csv', row.names = FALSE)
write.csv(DTTweets_Afinn,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_Afinn.csv', row.names = FALSE)

# Analyze the Histogram - Spread of net sentiment score for 5741 tweets
summary(DTTweets_Afinn)
hist(DTTweets_Afinn$net_sentiment)

DTTweets_AfinnCount <- DTTweets_Afinn %>% 
  count(net_sentiment)
DTTweets_AfinnCount <- as.data.frame(DTTweets_AfinnCount)
DTTweets_AfinnCount


