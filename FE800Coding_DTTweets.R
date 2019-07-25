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
# THIS IS THE DATA SET WITH NET SENTIMENT SCORE FOR EACH TWEET (TIME_ID)
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


#############
#make a copy of DTTweets_Core as dt_tweets_copy
dt_tweets_copy <- DTTweets_Core

nrow(dt_tweets_copy)
# [1] 7151
ncol(dt_tweets_copy)
# [1] 4

colnames(dt_tweets_copy)
# [1] "time_id"    "date_time"  "tweet_text" "tweet_id"  


## read list of 505 s&p stocks (sp_constituents) downloaded from https://github.com/datasets/s-and-p-500-companies/blob/master/data/constituents.csv 
sp_constituents_csv <- read.csv("sp_constituents.csv",header = TRUE, stringsAsFactors=FALSE)

## see first rows of the dataset and check number of rows
head(sp_constituents_csv)
nrow(sp_constituents_csv)
# [1] 505

ncol(sp_constituents_csv)
# [1] 3

## Create a data frame with just s&p company names
sp_constituents_csv_Name <- data.frame(Name = sp_constituents_csv$Name)
nrow(sp_constituents_csv_Name)
summary(sp_constituents_csv_Name)
head(sp_constituents_csv_Name)
colnames(sp_constituents_csv_Name)


#####NEEDS FIXING
#add a column to dt_tweets_copy to flag where keyword from s&p file matches
#dt_tweets_copy$sp_matched_tweets <- sapply(dt_tweets_copy$text, function(x) paste(grep(x, sp_constituents_csv$Name), collapse = ","))
#dt_tweets_copy$sp_matched_tweets <- sapply(dt_tweets_copy$tweet_text, function(x) paste(grep(x, sp_constituents_csv_Name, value = TRUE), collapse = ","))

colnames(dt_tweets_copy)

# create a df where s&p companies are mentioned in a tweet
dt_tweets_copy_filter <- filter(dt_tweets_copy,
                           grepl('3m|a.o. smith|abbott|abbvie|accenture|activision|acuity brands|adobe|advance auto parts|advanced micro devices|aes|aetna|affiliated managers group|aflac|agilent|air products & chemicals|akamai|alaska air|albermarle|alexandria real estate|alexion|align technology|allegion|allergan|alliance data|alliant energy|allstate|google|altria|amazon|ameren|american airlines|american electric power|american express|amex|aig|american tower|american water|ameriprise financial|abc|ametek|amgen|amphenol|anadarko petroleum|analog devices|andeavor|ansys|anthem|aon|apache corporation|aiv|apple|applied materials|aptiv|archer daniels|arconic inc|arthur j. gallagher|assurant inc|at&t|autodesk|adp|autozone|avalon bay|avery dennison|baker hughes|ball corp|bank of america|b of a|baxter|bb&t|becton|berkshire hathaway|best buy|biogen|black rock|h&r block|boeing|booking holdngs|borgwarner|boston properties|boston scientific|brighthouse financial|bms|bristol myers|broadcom|brown forman|bfc|robinson worldwide|ca inc|cabot oil & gas|cadence design|campbell soup|capital one|cardinal heath|carmax|carnival|caterpillar|cboe global|cbre|cbs|celgene|centene|center point energy|century link|cerner|cf industries holdings|charles schwab|charter communications|chevron|chipotle|chubb limited|church & dwight|cigna|cimarex energy|cincinnati financial|cintas|cisco|citigroup|citizens financial|citrix systems|cme group|cms energy|coke|coca cola|cognizant technology|colgate palmolive|colgate|comcast|comerica|conagra|concho resources|conoco phillips|consolidated edison|constellation brands|corning|costco|coty|crown castle|csra|csx|cummins|cvs|dhi|danaher|darden restaurants|davita|deere|delta|dentsply sirona|devon energy|digital realty trust|discover financial|discovery|discovery inc|dish network|dollar general|dollar tree|dominion energy|dover corp|dow dupont|dr pepper|dte energy|duke energy|duke realty|dxc|etrade|eastman chemical|eaton|ebay|ecolab|edison international|edwards lifesciences|electronic arts|emerson electric|entergy|envision healthcare|eog|eqt|equifax|equinix|equity residential|essex property trust|estee lauder|everest group|eversource energy|exelon|expedia|expeditors international|express scripts|extra space storage|exxon|exxon mobil|f5 networks|facebook|fastenal|federal realty|fedex|fidelity national|fifth third|first energy|fiserv|flir|flowserve|fluor|fmc|foot locker|ford|fortive|fortune brands|franklin resources|freeport mcmoran|gap inc|garmin|gartner|general dynamics|ge|general electric|general growth properties|general mills|general motors|genuine parts|gilead sciences|global payments|goldman|goldman sachs|goodyear|grainger|halliburton|hanes|harley|harley davidson|harris|hartford financial|hasbro|hca|hcp|helmerich payne|henry schein|hess|hpe|hewlett packard|hilton|hologic|home depot|honeywell|hormel foods|host hotels|hp|humana|huntington bancshares|huntington ingalls|idexx|ihs markit|illinois tool works|illumina|incyte|ingersoll rand|intel corp|intercontinental exchange|ibm|international paper|interpublic group|fragrances intl|intuit|intuitive sugical|invesco|ipg photonic|iqvia holdings|iron mountain|j. b. hunt|jacobs engineering|smucker|j&j|johnson & johnson|johnson controls|jpmorgan|juniper networks|kansas city southern|kellogg|key corp|kimberly clark|kimco realty|kinder morgan|kla tencor|kohls|kraft|heinz|kroger|l brands|l-3 communications|labcorp|lam research|leggett platt|lennar|leucadia national|eli lilly|lincoln national|lkq|lockheed|lockheed martin|loews|lowes|lyondell basell|m&t bank|macerich|macys|marathon oil|marathon petroleum|marriott|marsh|martin marietta|masco|mastercard|mattel|mccormick & co|mcdonalds|mckesson corp|medtronic|merck|met life|mettler toledo|mgm|kors|microchip technology|micron technology|microsoft|midamerica apartments|mohawk industries|molson coors|mondelez|monsanto|monster beverage|moodys|morgan stanley|motorola|mylan|nasdaq|national oilwell|navient|nektar therapeutics|netapp|netflix|newell brands|newfield exploration|newmont mining|news corp|next era|nielsen|nike|nisource|noble energy|nordstrom|norfolk southern|northern trust|northrop grumman|norwegian cruise|nrg|nucor|nvidia|oreilly|occidental petroleum|omnicom group|oneok|oracle|paccar|packaging corporation of america|parker hannifin|paychex|paypal|pentair|peoples united|pepsico|perkinelmer|perrigo|pfizer|pg&e|Phillip morris|phillips|pinnacle west|pioneer natural|pnc|ralph lauren|ppg|ppl|praxair|principal financial|p&g|procter & gamble|progressive corp|prologis|prudential|prudential financial|public serv enterprise|public storage|pulte homes|pvh|qorvo|qualcomm|quanta services|quest diagnostics|range resources|raymond james|raytheon|realty income|red hat|regency centers|regeneron|regions financial|republic services|resmed|robert half|rockwell automation|rockwell collins|roper technologies|ross stores|royal caribbean|s&p global|salesforce.com|sba|scana|schlumberger|seagate technology|sealed air|sempra energy|sherwin williams|simon property|skyworks solutions|slg|snap on|southern co|southwest airlines|black & decker|starbucks|state street|stericycle|stryker|suntrust|svb|symantec|synchrony financial|synopsys|sysco|rowe price|take-two|tapestry|target corp|te connectivty|technipfmc|texas instruments|textron|new york mellon|clorox|cooper|hershey|mosaic company|travelers company|disney|thermo fisher|tiffany & co|time warner|tjx|torchmark|tss|tractor supply|trans digm|trip advisor|century fox|tyson foods|u.s. bancorp|udr|ulta beauty|under armour|union pacific|united continental|united health|ups|united rentals|united technologies|uhs|unum group|v.f. corp|valero|varian medical|ventas|verisign|verisk analytics|verizon|vertex|viacom|visa|vornado|vulcan materials|walmart|walgreens|waste management|waters corporation|wec|wells fargo|welltower|western digital|western union|west rock|weyerhaeuser|whirlpool|williams cos|willis towers|wyndham|wynn resorts|xcel energy|xerox|xilinx|xl capital|xylem|yum brands|zimmer biomet|zions bancorp|zoetis', tweet_text))


head(dt_tweets_copy_filter)
nrow(dt_tweets_copy_filter)


###############scratch area beyond this point#####################
#keep <- which(!names(sp_constituents_csv$Name) %in% names(dt_tweets_copy$tweet_text))
#dt_tweets_copy[, keep]
#nrow(keep)
#head(keep)
