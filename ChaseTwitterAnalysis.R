#Sardar Afra
#@Chase Acoount Analysis
#09/20/2017

rm(list = ls())

# Instal Required packages ------------------------------------------------

installPkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Tweeter 1 ---------------------------------------------------------------
required_packages = c("twitteR","RCurl", "RJSONIO", "stringr","httr","rjson","tm","gridExtra",
                      "lubridate","wordcloud","SnowballC","magrittr","XML","tweetscores","foreign",
                      "syuzhet","ggplot2","arules")
installPkg(required_packages)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "Your_Consumer_Key"
consumer_secret <- "Your_Consumer_Secret"
access_token <- "Your_Access_Token"
access_secret <- "Your_Access_Secret"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Extract tweets from a single user at a time   
chase_tweets = userTimeline(user = "chase",
                            n = 10000, includeRts = TRUE, retryOnRateLimit = 2000)
chase.df = twListToDF(chase_tweets)
save(chase.df, file = "chaseAccountTweets.RData")

tweetsToChase = searchTwitter("@Chase", n = 1e4, lang="en", retryOnRateLimit = 2000)
#Put the tweets downloaded into a data.frame
tweetsToChase.df = twListToDF(tweetsToChase)
save(tweetsToChase.df, file = "lastTweetsToChase.RData")

# Pre-Processing ----------------------------------------------------------

# Hashtags 
tweetsToChase.df$hashtags = regmatches(tweetsToChase.df$text, gregexpr('#\\w+', tweetsToChase.df$text))
tweetsToChase.df$atChase3 = regmatches(tweetsToChase.df$text, gregexpr('@Chase+', tweetsToChase.df$text))
tweetsToChase.df$atChase4 = regmatches(tweetsToChase.df$text, gregexpr('@chase+', tweetsToChase.df$text))
tweetsToChase.df.clnd = tweetsToChase.df[tweetsToChase.df$atChase4 == "@chase" | tweetsToChase.df$atChase3 == "@Chase",]

# Build a sparse matrix of hashtags
library(arules)

ms <- regmatches(tweetsToChase.df.clnd$text, gregexpr("#(\\d|\\w)+", tweetsToChase.df.clnd$text))
im <- as(ms, "itemMatrix")
hashtagMatrix =as(im,"matrix")

# Analysis ----------------------------------------------------------------

tweet_txt = sapply(tweetsToChase, function(x) x$getText())

#Clean the tweets from stop words and other punctuation stuff :)
cleanTweets = function(tweet_txt){
  # remove retweet entities
  tweet_txt = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', tweet_txt)
  # remove at people
  tweet_txt = gsub('@\\w+', '', tweet_txt)
  # remove punctuation
  tweet_txt = gsub('[[:punct:]]', '', tweet_txt)
  # remove numbers
  tweet_txt = gsub('[[:digit:]]', '', tweet_txt)
  # remove html links
  tweet_txt = gsub('http\\w+', '', tweet_txt)
  # remove unnecessary spaces
  tweet_txt = gsub('[ \t]{2,}', '', tweet_txt)
  tweet_txt = gsub('^\\s+|\\s+$', '', tweet_txt)
  
  # define 'tolower error handling' function
  try.error = function(x)
  {
    # create missing value
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(try_error, 'error'))
      y = tolower(x)
    # result
    return(y)
  }
  # lower case using try.error with sapply
  tweet_txt = sapply(tweet_txt, try.error)
  
  # remove NAs in some_txt
  tweet_txt = tweet_txt[!is.na(tweet_txt)]
  names(tweet_txt) = NULL
  
  tweet_txt
}

tweet_txt = cleanText(tweet_txt)

# Sentiment Analysis ------------------------------------------------------
# Perform Sentiment Analysis
# classify emotion
tweet_emotion = classify_emotion(tweet_txt, algorithm='bayes', prior=1.0)

# get emotion best fit
emotion = tweet_emotion[,7]

# substitute NA's by 'unknown'
emotion[is.na(emotion)] = 'unknown'

# classify polarity
tweet_polarity = classify_polarity(tweet_txt, algorithm='bayes')

# get polarity best fit
polarity = tweet_polarity[,4]

# Create data frame with the results and obtain some general statistics
# data frame with results
res_df = data.frame(text = tweet_txt, emotion = emotion,
                     polarity = polarity, stringsAsFactors=FALSE)

# sort data frame
res_df = within(res_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

#Plot the polarity and emotion graphs
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity),width = 0.3) +
  # scale_fill_brewer(palette='RdGy') +
  scale_fill_brewer(palette='Set2') +
  # scale_fill_manual(values = c("#C0C0C0", "#98AFC7", "#6698FF"))
  labs(x='polarity categories', y='number of tweets') +
  ggtitle('Sentiment Analysis of Tweets about Chase\n(classification by polarity)') +
  theme(plot.title = element_text(size=12, face='bold'))

ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion),width = 0.3) +
  # scale_fill_brewer(palette='RdGy') +
  scale_fill_brewer(palette='Set2') +
  # scale_fill_manual(values = c("#C0C0C0", "#98AFC7", "#6698FF"))
  labs(x='emotion categories', y='number of tweets') +
  ggtitle('Sentiment Analysis of Tweets about Chase\n(classification by polarity)') +
  theme(plot.title = element_text(size=12, face='bold'))

#END
