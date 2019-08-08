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
# setwd("C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS")
# setwd("C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS")
setwd("C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS")

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
DTTweets_CSV$minuteID <- as.numeric(DTTweets_CSV$minuteID)
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

DTTweets_CSV$minuteID == DTTweets_CSV$timeID

## Export CSV
#write.csv(DTTweets_CSV,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_ID.csv', row.names = FALSE)
#write.csv(DTTweets_CSV,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_ID.csv', row.names = FALSE)
write.csv(DTTweets_CSV,'C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS/DTTweets_ID.csv', row.names = FALSE)

## Create a data frame with just Core data - identifiers and tweet text
DTTweets_Core <- data.frame(minute_id = DTTweets_CSV$minuteID,
                            time_id = DTTweets_CSV$timeID,
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

write.csv(DTTweets_Core,'C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS/DTTweets_Core.csv', row.names = FALSE)


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
## [1] 190501 / 18270

DTTweets_Words_clean <- DTTweets_Words %>%
  anti_join(stop_words) %>%
  filter(!word == "rt")

# how many words after removing the stop words?
nrow(DTTweets_Words_clean)
## [1] 79035 / 8099


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
# DTTweets_Pair_Counts <-DTTweets_Pair %>%
#   count(paired_words, sort = TRUE)
# 
# as.data.frame(DTTweets_Pair_Counts[1:30,])
# 
# ## Separate Words into Columns
# DTTweets_PairSeparated <- DTTweets_Pair %>%
#   separate(paired_words, c("word1", "word2"), sep = " ")
# 
# DTTweets_PairSeparated[1:30,]
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

# Consolidate Sentiment values by Tweet using groupby() function
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
colnames(DTTweets_BingBySentiment)

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

# Analyze the data set
summary(DTTweets_Bing)
hist(DTTweets_Bing$net_sentiment)

# The net sentiment contains 0 and should be normalized to a postiive scale by adding 7 (lowest value is -6)
mode(DTTweets_Bing$net_sentiment)
DTTweets_Bing$normalized_sentiment <- DTTweets_Bing$net_sentiment + 7
mode(DTTweets_Bing$normalized_sentiment)

# Analyze the Histogram - Spread of Normalized sentiment score 
hist(DTTweets_Bing$normalized_sentiment)
summary(DTTweets_Bing$normalized_sentiment)
nrow(DTTweets_Bing)


# THIS IS THE DATA SET WITH NET SENTIMENT SCORE - USING BING - FOR EACH TWEET (TIME_ID)
# THIS SHOULD FEED INTO STOCK ANALYSIS

# Export the data set with Net Sentiment Score
write.csv(DTTweets_Bing,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_Bing.csv', row.names = FALSE)
# write.csv(DTTweets_Bing,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_Bing.csv', row.names = FALSE)

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


# Review the Text of Positive Tweets 

nrow(DTTweets_Core)
nrow(DTTweets_BingPositive)
DTTweets_BingPositive$time_id
mode(DTTweets_BingPositive$time_id)
DTTweets_Core$time_id
mode(DTTweets_Core$time_id)

DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_BingPositive$time_id)]


# Review the Text of Negative Tweets 
DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_BingNegative$time_id)]

# Review the Text of Tweets with Negative score of -5 or lower 
DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_BingNegative5$time_id)]



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

# Analyze the Histogram - Spread of net sentiment score 
summary(DTTweets_Afinn)
hist(DTTweets_Afinn$net_sentiment)

DTTweets_AfinnCount <- DTTweets_Afinn %>% 
  count(net_sentiment)
DTTweets_AfinnCount <- as.data.frame(DTTweets_AfinnCount)
DTTweets_AfinnCount


# The net sentiment contains 0 and should be normalized to a postiive scale by adding 15 (lowest value is -14)
mode(DTTweets_Afinn$net_sentiment)
DTTweets_Afinn$normalized_sentiment <- DTTweets_Afinn$net_sentiment + 15
mode(DTTweets_Afinn$normalized_sentiment)

# Analyze the Histogram - Spread of Normalized sentiment score 
hist(DTTweets_Afinn$normalized_sentiment)
summary(DTTweets_Afinn$normalized_sentiment)
nrow(DTTweets_Afinn)


# THIS IS THE DATA SET WITH NET SENTIMENT SCORE - USING AFINN - FOR EACH TWEET (TIME_ID)
# THIS SHOULD FEED INTO STOCK ANALYSIS

# Export the data set with Net Sentiment Score
# write.csv(DTTweets_Afinn,'C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS/DTTweets_Afinn.csv', row.names = FALSE)
write.csv(DTTweets_Afinn,'C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS/DTTweets_Afinn.csv', row.names = FALSE)


# Filter Tweets with positive values
DTTweets_AfinnPositive <- DTTweets_Afinn %>%
  filter(net_sentiment > 0) # only select rows with net POSITIVE sentiment score 
DTTweets_AfinnNegative <- DTTweets_Afinn %>%
  filter(net_sentiment < 0) # only select rows with net NEGATIVE sentiment score
nrow(DTTweets_AfinnPositive)
nrow(DTTweets_AfinnNegative)
DTTweets_AfinnNegative5 <- DTTweets_Afinn %>%
  filter(net_sentiment <= -5) # only select rows with net NEGATIVE sentiment score of 5 or lower


# Review the Text of Positive Tweets 

nrow(DTTweets_Core)
nrow(DTTweets_AfinnPositive)
DTTweets_AfinnPositive$time_id
mode(DTTweets_AfinnPositive$time_id)
DTTweets_Core$time_id
mode(DTTweets_Core$time_id)

DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_AfinnPositive$time_id)]


# Review the Text of Negative Tweets 
DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_AfinnNegative$time_id)]

# Review the Text of Tweets with Negative score of -5 or lower 
DTTweets_Core$tweet_text[which(DTTweets_Core$time_id %in% DTTweets_AfinnNegative5$time_id)]


## *********************************
### DO IMPACT ANALYSIS ON STOCK RETURNS 
## INTEGRATE RICH's CODE
## *********************************

##  INSTALL REQUIRED PACKAGES AND LIBRARIES
install.packages("sm")
install.packages("ggplot2")
install.packages("sqldf")
# install.packages("gsubfn")
# install.packages("proto")
# install.packages("RSQLite")
# install.packages("blob")
# library(blob)

##  INSTALL LIBRARIES
library(sm)
library(ggplot2)
library(sqldf)
# library(gsubfn)
# library(proto)
# library(RSQLite)

## SET WORKING DIRECTORY}

getwd()
#setwd("C:/Users/harshil.b.shah/Documents/GitHub/FE800HBS")
#setwd("C:/Users/binta.d.patel/Documents/GitHub/FE800HBS/FE800HBS")
setwd("C:/Users/richa/OneDrive/Documents/Education/Stevens Institute/FE 800/Project/FE800HBS")

## read Donald Trump tweets downloaded from trumptwitterarchive.com 
realDT <- read.csv("DonaldTrumpTweets.csv",header = TRUE)

## LOAD STOCK DATA
##  Read Stock Data
stockdata <- read.csv("stockdata.csv", header = TRUE, sep = ",")
##  Summarize Stock Data
head(stockdata)
##  Number of Rows in stock market data
nrow(stockdata)

## SUBSET STOCK DATA
## CREATE SUBSETS OF THE STOCK DATA FOR DIFFERENT ANALYISS

overnight_ret_1_min <- subset(stockdata, overnight_1 == 1) ## select the subject of returns that are from close of business to opening of next business day
ret_1_min <- subset(stockdata, overnight_1 == 0) ## select the subset of 1 minute returns that are intraday
ret_2_min <- subset(stockdata, overnight_2 == 0) ## select the subset of 2 minute returns that are intraday
ret_5_min <- subset(stockdata, overnight_5 == 0) ## select the subset of 5 minute returns that are intraday
ret_10_min <- subset(stockdata, overnight_10 == 0) ## select the subset of 10 minute returns that are intraday
ret_20_min <- subset(stockdata, overnight_20 == 0) ## select the subset of 20 minute returns that are intraday
ret_30_min <- subset(stockdata, overnight_30 == 0) ## select the subset of 30 minute returns that are intraday
ret_60_min <- subset(stockdata, overnight_60 == 0) ## select the subset of 60 minute returns that are intraday
ret_120_min <- subset(stockdata, overnight_120 == 0) ## select the subset of 120 minute returns that are intraday
ret_240_min <- subset(stockdata, overnight_240 == 0) ## select the subset of 240 minute returns that are intraday
ret_360_min <- subset(stockdata, overnight_360 == 0) ## select the subset of 360 minute returns that are intraday

## STATISTICS
##  Statistics about 1, 2, 5, 10, 20, 30, 60, 120, 240, and 360-minute returns
max1 <- max(ret_1_min$ret_1_min) #max return
min1 <- min(ret_1_min$ret_1_min) #min return
mean1 <- mean(ret_1_min$ret_1_min) #mean
sd1 <- sd(ret_1_min$ret_1_min) #standard deviation

max2 <- max(ret_2_min$ret_2_min) #max return
min2 <- min(ret_2_min$ret_2_min) #min return
mean2 <- mean(ret_2_min$ret_2_min) #mean
sd2 <- sd(ret_2_min$ret_2_min) #standard deviation

max5 <- max(ret_5_min$ret_5_min) #max return
min5 <- min(ret_5_min$ret_5_min) #min return
mean5 <- mean(ret_5_min$ret_5_min) #mean
sd5 <- sd(ret_5_min$ret_5_min) #standard deviation

max10 <- max(ret_10_min$ret_10_min) #max return
min10 <- min(ret_10_min$ret_10_min) #min return
mean10 <- mean(ret_10_min$ret_10_min) #mean
sd10 <- sd(ret_10_min$ret_10_min) #standard deviation

max20 <- max(ret_20_min$ret_20_min) #max return
min20 <- min(ret_20_min$ret_20_min) #min return
mean20 <- mean(ret_20_min$ret_20_min) #mean
sd20 <- sd(ret_20_min$ret_20_min) #standard deviation

max30 <- max(ret_30_min$ret_30_min) #max return
min30 <- min(ret_30_min$ret_30_min) #min return
mean30 <- mean(ret_30_min$ret_30_min) #mean
sd30 <- sd(ret_30_min$ret_30_min) #standard deviation

max60 <- max(ret_60_min$ret_60_min) #max return
min60 <- min(ret_60_min$ret_60_min) #min return
mean60 <- mean(ret_60_min$ret_60_min) #mean
sd60 <- sd(ret_60_min$ret_60_min) #standard deviation

max120 <- max(ret_120_min$ret_120_min) #max return
min120 <- min(ret_120_min$ret_120_min) #min return
mean120 <- mean(ret_120_min$ret_120_min) #mean
sd120 <- sd(ret_120_min$ret_120_min) #standard deviation

max240 <- max(ret_240_min$ret_240_min) #max return
min240 <- min(ret_240_min$ret_240_min) #min return
mean240 <- mean(ret_240_min$ret_240_min) #mean
sd240 <- sd(ret_240_min$ret_240_min) #standard deviation

max360 <- max(ret_360_min$ret_360_min) #max return
min360 <- min(ret_360_min$ret_360_min) #min return
mean360 <- mean(ret_360_min$ret_360_min) #mean
sd360 <- sd(ret_360_min$ret_360_min) #standard deviation

maxon <- max(overnight_ret_1_min$ret_1_min, na.rm=TRUE) #max return (excludes NA values)
minon <- min(overnight_ret_1_min$ret_1_min, na.rm=TRUE) #min return (excludes NA values)
meanon <- mean(overnight_ret_1_min$ret_1_min, na.rm=TRUE) #mean (excludes NA values)
sdon <- sd(overnight_ret_1_min$ret_1_min, na.rm=TRUE) #standard deviation (excludes NA values)

## SUMMARY TABLE WITH STATISTICS RESULTS}
##  Create Summary Table

sumtable <- matrix(c(max1, min1, mean1, sd1, max2, min2, mean2, sd2, max5, min5, mean5, sd5, max10, min10, mean10, sd10, max20, min20, mean20, sd20, max30, min30, mean30, sd30, max60, min60, mean60, sd60, max120, min120, mean120, sd120, max240, min240, mean240, sd240, max360, min360, mean360, sd360, maxon, minon, meanon, sdon), ncol = 4, byrow = TRUE)
colnames(sumtable) <- c("Max", "Min", "Mean", "Std. Dev")
rownames(sumtable) <- c("1 Minute", "2 Minutes", "5 Minutes", "10 Minutes", "20 Minutes", "30 Minutes", "60 Minutes", "120 Minutes", "240 Minutes", "360 Minutes", "Overnight")
sumtable <- as.table(sumtable)
sumtable

## DT TWEET STOCKDATA
## CREATE SUBSETS OF THE STOCK DATA THAT REPRESENTS THE TIME FRAMES AFTER DONALD TRUMP TWEETS

dt_overnight_ret_1_min <- subset(stockdata, dttweetflag == 1 & overnight_1 == 1) ## select the subset of returns that are from close of business to opening of next business day
dt_ret_1_min <- subset(stockdata, dttweetflag == 1 & overnight_1 == 0) ## select the subset of 1 minute returns that are intraday
dt_ret_2_min <- subset(stockdata, dttweetflag == 1 & overnight_2 == 0) ## select the subset of 2 minute returns that are intraday
dt_ret_5_min <- subset(stockdata, dttweetflag == 1 & overnight_5 == 0) ## select the subset of 5 minute returns that are intraday
dt_ret_10_min <- subset(stockdata, dttweetflag == 1 & overnight_10 == 0) ## select the subset of 10 minute returns that are intraday
dt_ret_20_min <- subset(stockdata, dttweetflag == 1 & overnight_20 == 0) ## select the subset of 20 minute returns that are intraday
dt_ret_30_min <- subset(stockdata, dttweetflag == 1 & overnight_30 == 0) ## select the subset of 30 minute returns that are intraday
dt_ret_60_min <- subset(stockdata, dttweetflag == 1 & overnight_60 == 0) ## select the subset of 60 minute returns that are intraday
dt_ret_120_min <- subset(stockdata, dttweetflag == 1 & overnight_120 == 0) ## select the subset of 120 minute returns that are intraday
dt_ret_240_min <- subset(stockdata, dttweetflag == 1 & overnight_240 == 0) ## select the subset of 240 minute returns that are intraday
dt_ret_360_min <- subset(stockdata, dttweetflag == 1 & overnight_360 == 0) ## select the subset of 360 minute returns that are intraday

## STATISTICS DT
##  Statistics about 1, 2, 5, 10, 20, 30, 60, 120, 240, and 360-minute returns
dt_max1 <- max(dt_ret_1_min$ret_1_min) #max return
dt_min1 <- min(dt_ret_1_min$ret_1_min) #min return
dt_mean1 <- mean(dt_ret_1_min$ret_1_min) #mean
dt_sd1 <- sd(dt_ret_1_min$ret_1_min) #standard deviation

dt_max2 <- max(dt_ret_2_min$ret_2_min) #max return
dt_min2 <- min(dt_ret_2_min$ret_2_min) #min return
dt_mean2 <- mean(dt_ret_2_min$ret_2_min) #mean
dt_sd2 <- sd(dt_ret_2_min$ret_2_min) #standard deviation

dt_max5 <- max(dt_ret_5_min$ret_5_min) #max return
dt_min5 <- min(dt_ret_5_min$ret_5_min) #min return
dt_mean5 <- mean(dt_ret_5_min$ret_5_min) #mean
dt_sd5 <- sd(dt_ret_5_min$ret_5_min) #standard deviation

dt_max10 <- max(dt_ret_10_min$ret_10_min) #max return
dt_min10 <- min(dt_ret_10_min$ret_10_min) #min return
dt_mean10 <- mean(dt_ret_10_min$ret_10_min) #mean
dt_sd10 <- sd(dt_ret_10_min$ret_10_min) #standard deviation

dt_max20 <- max(dt_ret_20_min$ret_20_min) #max return
dt_min20 <- min(dt_ret_20_min$ret_20_min) #min return
dt_mean20 <- mean(dt_ret_20_min$ret_20_min) #mean
dt_sd20 <- sd(dt_ret_20_min$ret_20_min) #standard deviation

dt_max30 <- max(dt_ret_30_min$ret_30_min) #max return
dt_min30 <- min(dt_ret_30_min$ret_30_min) #min return
dt_mean30 <- mean(dt_ret_30_min$ret_30_min) #mean
dt_sd30 <- sd(dt_ret_30_min$ret_30_min) #standard deviation

dt_max60 <- max(dt_ret_60_min$ret_60_min) #max return
dt_min60 <- min(dt_ret_60_min$ret_60_min) #min return
dt_mean60 <- mean(dt_ret_60_min$ret_60_min) #mean
dt_sd60 <- sd(dt_ret_60_min$ret_60_min) #standard deviation

dt_max120 <- max(dt_ret_120_min$ret_120_min) #max return
dt_min120 <- min(dt_ret_120_min$ret_120_min) #min return
dt_mean120 <- mean(dt_ret_120_min$ret_120_min) #mean
dt_sd120 <- sd(dt_ret_120_min$ret_120_min) #standard deviation

dt_max240 <- max(dt_ret_240_min$ret_240_min) #max return
dt_min240 <- min(dt_ret_240_min$ret_240_min) #min return
dt_mean240 <- mean(dt_ret_240_min$ret_240_min) #mean
dt_sd240 <- sd(dt_ret_240_min$ret_240_min) #standard deviation

dt_max360 <- max(dt_ret_360_min$ret_360_min) #max return
dt_min360 <- min(dt_ret_360_min$ret_360_min) #min return
dt_mean360 <- mean(dt_ret_360_min$ret_360_min) #mean
dt_sd360 <- sd(dt_ret_360_min$ret_360_min) #standard deviation

dt_maxon <- max(dt_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #max return (excludes NA values)
dt_minon <- min(dt_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #min return (excludes NA values)
dt_meanon <- mean(dt_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #mean (excludes NA values)
dt_sdon <- sd(dt_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #standard deviation (excludes NA values)

## SUMMARY TABLE WITH STATISTICS RESULTS DT}
##  Create Summary Table

dt_sumtable <- matrix(c(dt_max1, dt_min1, dt_mean1, dt_sd1, dt_max2, dt_min2, dt_mean2, dt_sd2, dt_max5, dt_min5, dt_mean5, dt_sd5, dt_max10, dt_min10, dt_mean10, dt_sd10, dt_max20, dt_min20, dt_mean20, dt_sd20, dt_max30, dt_min30, dt_mean30, dt_sd30, dt_max60, dt_min60, dt_mean60, dt_sd60, dt_max120, dt_min120, dt_mean120, dt_sd120, dt_max240, dt_min240, dt_mean240, dt_sd240, dt_max360, dt_min360, dt_mean360, dt_sd360, dt_maxon, dt_minon, dt_meanon, dt_sdon), ncol = 4, byrow = TRUE)
colnames(dt_sumtable) <- c("Max", "Min", "Mean", "Std. Dev")
rownames(dt_sumtable) <- c("1 Minute", "2 Minutes", "5 Minutes", "10 Minutes", "20 Minutes", "30 Minutes", "60 Minutes", "120 Minutes", "240 Minutes", "360 Minutes", "Overnight")
dt_sumtable <- as.table(dt_sumtable)
dt_sumtable

## Data Preparation for Key Word Analysis
## CREATE SUBSETS OF THE STOCK DATA THAT REPRESENTS THE TIME FRAMES AFTER DONALD TRUMP TWEETS

dtecon_overnight_ret_1_min <- subset(stockdata, dt_econ_flg == 0 & dt_econ_on_flag == 1) ## select the subset of returns that are from close of business to opening of next business day
dtecon_ret_1_min <- subset(stockdata, dt_econ_flg == 1 & overnight_1 == 0) ## select the subset of 1 minute returns that are intraday
dtecon_ret_2_min <- subset(stockdata, dt_econ_flg == 1 & overnight_2 == 0) ## select the subset of 2 minute returns that are intraday
dtecon_ret_5_min <- subset(stockdata, dt_econ_flg == 1 & overnight_5 == 0) ## select the subset of 5 minute returns that are intraday
dtecon_ret_10_min <- subset(stockdata, dt_econ_flg == 1 & overnight_10 == 0) ## select the subset of 10 minute returns that are intraday
dtecon_ret_20_min <- subset(stockdata, dt_econ_flg == 1 & overnight_20 == 0) ## select the subset of 20 minute returns that are intraday
dtecon_ret_30_min <- subset(stockdata, dt_econ_flg == 1 & overnight_30 == 0) ## select the subset of 30 minute returns that are intraday
dtecon_ret_60_min <- subset(stockdata, dt_econ_flg == 1 & overnight_60 == 0) ## select the subset of 60 minute returns that are intraday
dtecon_ret_120_min <- subset(stockdata, dt_econ_flg == 1 & overnight_120 == 0) ## select the subset of 120 minute returns that are intraday
dtecon_ret_240_min <- subset(stockdata, dt_econ_flg == 1 & overnight_240 == 0) ## select the subset of 240 minute returns that are intraday
dtecon_ret_360_min <- subset(stockdata, dt_econ_flg == 1 & overnight_360 == 0) ## select the subset of 360 minute returns that are intraday

##  Statistics about 1, 2, 5, 10, 20, 30, 60, 120, 240, and 360-minute returns
dtecon_max1 <- max(dtecon_ret_1_min$ret_1_min) #max return
dtecon_min1 <- min(dtecon_ret_1_min$ret_1_min) #min return
dtecon_mean1 <- mean(dtecon_ret_1_min$ret_1_min) #mean
dtecon_sd1 <- sd(dtecon_ret_1_min$ret_1_min) #standard deviation

dtecon_max2 <- max(dtecon_ret_2_min$ret_2_min) #max return
dtecon_min2 <- min(dtecon_ret_2_min$ret_2_min) #min return
dtecon_mean2 <- mean(dtecon_ret_2_min$ret_2_min) #mean
dtecon_sd2 <- sd(dtecon_ret_2_min$ret_2_min) #standard deviation

dtecon_max5 <- max(dtecon_ret_5_min$ret_5_min) #max return
dtecon_min5 <- min(dtecon_ret_5_min$ret_5_min) #min return
dtecon_mean5 <- mean(dtecon_ret_5_min$ret_5_min) #mean
dtecon_sd5 <- sd(dtecon_ret_5_min$ret_5_min) #standard deviation

dtecon_max10 <- max(dtecon_ret_10_min$ret_10_min) #max return
dtecon_min10 <- min(dtecon_ret_10_min$ret_10_min) #min return
dtecon_mean10 <- mean(dtecon_ret_10_min$ret_10_min) #mean
dtecon_sd10 <- sd(dtecon_ret_10_min$ret_10_min) #standard deviation

dtecon_max20 <- max(dtecon_ret_20_min$ret_20_min) #max return
dtecon_min20 <- min(dtecon_ret_20_min$ret_20_min) #min return
dtecon_mean20 <- mean(dtecon_ret_20_min$ret_20_min) #mean
dtecon_sd20 <- sd(dtecon_ret_20_min$ret_20_min) #standard deviation

dtecon_max30 <- max(dtecon_ret_30_min$ret_30_min) #max return
dtecon_min30 <- min(dtecon_ret_30_min$ret_30_min) #min return
dtecon_mean30 <- mean(dtecon_ret_30_min$ret_30_min) #mean
dtecon_sd30 <- sd(dtecon_ret_30_min$ret_30_min) #standard deviation

dtecon_max60 <- max(dtecon_ret_60_min$ret_60_min) #max return
dtecon_min60 <- min(dtecon_ret_60_min$ret_60_min) #min return
dtecon_mean60 <- mean(dtecon_ret_60_min$ret_60_min) #mean
dtecon_sd60 <- sd(dtecon_ret_60_min$ret_60_min) #standard deviation

dtecon_max120 <- max(dtecon_ret_120_min$ret_120_min) #max return
dtecon_min120 <- min(dtecon_ret_120_min$ret_120_min) #min return
dtecon_mean120 <- mean(dtecon_ret_120_min$ret_120_min) #mean
dtecon_sd120 <- sd(dtecon_ret_120_min$ret_120_min) #standard deviation

dtecon_max240 <- max(dtecon_ret_240_min$ret_240_min) #max return
dtecon_min240 <- min(dtecon_ret_240_min$ret_240_min) #min return
dtecon_mean240 <- mean(dtecon_ret_240_min$ret_240_min) #mean
dtecon_sd240 <- sd(dtecon_ret_240_min$ret_240_min) #standard deviation

dtecon_max360 <- max(dtecon_ret_360_min$ret_360_min) #max return
dtecon_min360 <- min(dtecon_ret_360_min$ret_360_min) #min return
dtecon_mean360 <- mean(dtecon_ret_360_min$ret_360_min) #mean
dtecon_sd360 <- sd(dtecon_ret_360_min$ret_360_min) #standard deviation

dtecon_maxon <- max(dtecon_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #max return (excludes NA values)
dtecon_minon <- min(dtecon_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #min return (excludes NA values)
dtecon_meanon <- mean(dtecon_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #mean (excludes NA values)
dtecon_sdon <- sd(dtecon_overnight_ret_1_min$ret_1_min, na.rm=TRUE) #standard deviation (excludes NA values)

##  Create Summary Table

dtecon_sumtable <- matrix(c(dtecon_max1, dtecon_min1, dtecon_mean1, dtecon_sd1, dtecon_max2, dtecon_min2, dtecon_mean2, dtecon_sd2, dtecon_max5, dtecon_min5, dtecon_mean5, dtecon_sd5, dtecon_max10, dtecon_min10, dtecon_mean10, dtecon_sd10, dtecon_max20, dtecon_min20, dtecon_mean20, dtecon_sd20, dtecon_max30, dtecon_min30, dtecon_mean30, dtecon_sd30, dtecon_max60, dtecon_min60, dtecon_mean60, dtecon_sd60, dtecon_max120, dtecon_min120, dtecon_mean120, dtecon_sd120, dtecon_max240, dtecon_min240, dtecon_mean240, dtecon_sd240, dtecon_max360, dtecon_min360, dtecon_mean360, dtecon_sd360, dtecon_maxon, dtecon_minon, dtecon_meanon, dtecon_sdon), ncol = 4, byrow = TRUE)
colnames(dtecon_sumtable) <- c("Max", "Min", "Mean", "Std. Dev")
rownames(dtecon_sumtable) <- c("1 Minute", "2 Minutes", "5 Minutes", "10 Minutes", "20 Minutes", "30 Minutes", "60 Minutes", "120 Minutes", "240 Minutes", "360 Minutes", "Overnight")
dtecon_sumtable <- as.table(dtecon_sumtable)
dtecon_sumtable

## HISTOGRAMS

## HISTOGRAM OF MINUTE BY MINUTE STOCK RETURNS

hist(ret_1_min$ret_1_min, 
     breaks = 500,
     main = "1 Minute Intraday Returns",
     xlab = "Minute Level Returns", 
     col = "Light Blue")

##  HISTOGRAM OF OVERNIGHT STOCK RETURNS

hist(overnight_ret_1_min$ret_1_min, 
     breaks = 100,
     main = "Overnight Returns",
     xlab = "End of Trading Day to Beginning of Next Trading Day Returns", 
     col = "Red")

## DENSITY PLOTS

##  CREATE DENSITY PLOT DISTRIBUTIONS

d1 <- density(ret_1_min$ret_1_min, from = -0.05, to = 0.05)
d2 <- density(ret_2_min$ret_2_min, from = -0.05, to = 0.05)
d5 <- density(ret_5_min$ret_5_min, from = -0.05, to = 0.05)
d10 <- density(ret_10_min$ret_10_min, from = -0.05, to = 0.05)
d20 <- density(ret_20_min$ret_20_min, from = -0.05, to = 0.05)
d30 <- density(ret_30_min$ret_30_min, from = -0.05, to = 0.05)
d60 <- density(ret_60_min$ret_60_min, from = -0.05, to = 0.05)
d120 <- density(ret_120_min$ret_120_min, from = -0.05, to = 0.05)
d240 <- density(ret_240_min$ret_240_min, from = -0.05, to = 0.05)
d360 <- density(ret_360_min$ret_360_min, from = -0.05, to = 0.05)
don <- density(overnight_ret_1_min$ret_1_min, na.rm = TRUE, from = -0.05, to = 0.05) #EXCLUDE NA VALUES FROM ANALYSIS (ERROR RESULTS IF YOU DON'T DO THIS)


##  PLOT DENSITY PLOTS

plot(d1, main = "Denstiy Plot: 1 Minute Returns", xlab = "Distribution of 1 Minute Returns") 
polygon(d1, col = "Light Blue", border = "Blue")
plot(d2, main = "Denstiy Plot: 2 Minute Returns", xlab = "Distribution of 2 Minute Returns")
polygon(d2, col = "Light Blue", border = "Blue")
plot(d5, main = "Denstiy Plot: 5 Minute Returns", xlab = "Distribution of 5 Minute Returns")
polygon(d5, col = "Light Blue", border = "Blue")
plot(d10, main = "Denstiy Plot: 10 Minute Returns", xlab = "Distribution of 10 Minute Returns")
polygon(d10, col = "Light Blue", border = "Blue")
plot(d20, main = "Denstiy Plot: 20 Minute Returns", xlab = "Distribution of 20 Minute Returns")
polygon(d20, col = "Light Blue", border = "Blue")
plot(d30, main = "Denstiy Plot: 30 Minute Returns", xlab = "Distribution of 30 Minute Returns")
polygon(d30, col = "Light Blue", border = "Blue")
plot(d60, main = "Denstiy Plot: 60 Minute Returns", xlab = "Distribution of 60 Minute Returns")
polygon(d60, col = "Light Blue", border = "Blue")
plot(d120, main = "Denstiy Plot: 120 Minute Returns", xlab = "Distribution of 120 Minute Returns")
polygon(d120, col = "Light Blue", border = "Blue")
plot(d240, main = "Denstiy Plot: 240 Minute Returns", xlab = "Distribution of 240 Minute Returns")
polygon(d240, col = "Light Blue", border = "Blue")
plot(d360, main = "Denstiy Plot: 360 Minute Returns", xlab = "Distribution of 360 Minute Returns")
polygon(d360, col = "Light Blue", border = "Blue")
plot(don, main = "Denstiy Plot: Overnight Returns", xlab = "Distribution of Overnight Returns")
polygon(don, col = "Light Green", border = "Green")

## LOAD TRUMP TWEETS

dt <- read.csv("dt_tweets.csv", sep = ",", header = TRUE)
dt

## TRUMP TWEET DATA PROFILING
##  DATA PROFILING USING SQLDF

##  Number of Tweets in the Data Set
sqldf("SELECT COUNT(*) FROM dt")

## COUNT OF TWEETS IN / OUT OF TRADING HOURS

##  Number of Tweets During / Outside of Trading Hours
sqldf("SELECT DURINGTRADINGHOURS, COUNT(*)
      FROM dt
      GROUP BY 1")

## BAR PLOT OF TRUMPS TWEETS PER HOUR


##  Number of Tweets Per Hour of the Day
DT1 <- sqldf('SELECT HOUR, COUNT(*) as numtweets 
      FROM dt 
      GROUP BY hour')

x <- 0:23 # NEEDED IN PLOT BELOW TO CREATE X AXIS

barplot(DT1$numtweets, 
        main = "Donald Trump Tweets by Hour of the Day",
        xlab = "Hour of the Day", 
        ylab = "Number of Tweets",
        col = "light blue",
        names.arg = c(x),
        cex.lab=1)

## Number of Tweets by Year and Month
DT2 <- sqldf('SELECT YEAR, MONTH, DATE, COUNT(*) as numtweets
      FROM dt
      GROUP BY 1, 2, 3')

plot(DT2$numtweets,type = "l", 
     main = "Number of Tweets by Day", 
     xlab = "Day", 
     ylab = "Number of Tweets", 
     col = "Blue")

