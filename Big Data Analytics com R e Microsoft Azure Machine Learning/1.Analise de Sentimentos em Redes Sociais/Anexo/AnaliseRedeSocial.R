# Análise de Sentimentos em Redes Sociais.

# Where is the project.
setwd("/home/oracy/Documents/DSA_Projetos/DSA_Projetos/Big Data Analytics com R e Microsoft Azure Machine Learning/1.Análise de Sentimentos em Redes Sociais/")
getwd()

## Step 1 - Authentication and Packages.

# Installing and loading twitteR package.
#install.packages("twitteR")
library(twitteR)

# Package to get Twitter APIs.
# Font: https://cran.r-project.org/web/packages/httr/index.html
#install.packages("httr")
library(httr)

# Other package
library(stringr)

# Loading my own library.
source('utils.R')

# Twitter authentication.
# Font: https://medium.com/@GalarnykMichael/accessing-data-from-twitter-api-using-r-part1-b387a1c7d3e
consumer <- "ZiB0QzMeBY0JFwQGZNisMrBuj"
consumerSecret <- "b8tfwK6bYTBOiQLKPOe4hLCs5kWFdSqtoQNDGhtk7PdC4laqAV"
accessToken <- "199032609-j4014nhYooOV8xDm6Ngl71jHNUGtcghkWhfIdr23"
accessSecret <- "pX1AffKYylkjqSUNNiSwaeVXWa0MF11ppA8SZ5PBco5j3"

# Twitter Authentication.
# Font: https://www.rdocumentation.org/packages/twitteR/versions/1.1.9/topics/setup_twitter_oauth
twitteR::setup_twitter_oauth(consumer, consumerSecret, accessToken, accessSecret)


## Step 2 - Connection and data gathering.

# Check user timeline if everything is going fine.
# Font: https://www.r-bloggers.com/visualising-twitter-user-timeline-activity-in-r/
#twitteR::userTimeline("elonmusk")

# Get tweets.
# Font: https://www.rdocumentation.org/packages/twitteR/versions/1.1.9/topics/searchTwitter

# SearchString
query <- "Trump"
# How many tweets will get
#quantity <- 500
# Which language
#language <- "pt"
# Since Date
#sinceDate <- "2018-11-14"
tweet <- twitteR::searchTwitter(query)#, since = sinceDate)

# Check the first 5 tweets.
head(tweet)


## Step 3 - Text mining

# Package for Text Mining.
# Font: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
# Font: https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/
#install.packages("tm")

# Font: https://cran.r-project.org/web/packages/SnowballC/SnowballC.pdf
#install.packages("SnowballC")
library(SnowballC)
library(tm)
library(stringr)
options(warn=-1)

# TM Cleaning, organizing and transformation
tweetlist <- sapply(tweet, function(x) x$getText())
tweetlist <- iconv(tweetlist, to = "utf-8", sub="")
tweetlist <- limpaTweets(tweetlist)
tweetcorpus <- VCorpus(VectorSource(tweetlist))
tweetcorpus <- tm_map(tweetcorpus, removePunctuation)
tweetcorpus <- tm_map(tweetcorpus, tolower)
#tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, c(stopwords("pt"), "lula")))
tweetcorpus <- tm_map(tweetcorpus, function(x)removeWords(x, c(stopwords("en"))))
# Test to see how it is going
strwrap(tweetcorpus[[1]])

# Should convert to plan text before to create the matrix.
tweetcorpusPlan <- tm_map(tweetcorpus, PlainTextDocument)
tweetListSecond = as.matrix(TermDocumentMatrix(tweetcorpusPlan), control = list(stopwords = c(stopwords("portuguese"))))


## Step 4 - Wordcloud, and dendograma
# Font: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know

# Install and load wordcloud and RColorBrewer packages
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
library(wordcloud)
library(RColorBrewer)

# Generate a wordcloud
pal2 <- brewer.pal(8,"Dark2")

wordcloud(tweetcorpusPlan, 
          min.freq = 2, 
          scale = c(5,1), 
          random.color = F, 
          random.order = F,
          colors = pal2)

# Convert text object to Matrix
tweetMatrix <- TermDocumentMatrix(tweetcorpusPlan)
tweetMatrix

# Find more frequent word
# Font: https://rdrr.io/rforge/tm/man/findMostFreqTerms.html
findMostFreqTerms(tweetMatrix)

# Search for Association
# Font: https://rdrr.io/rforge/tm/man/findAssocs.html
findAssocs(tweetMatrix, "maluco", 0.6)

# Removing sparse terms
# Font: https://stackoverflow.com/questions/28763389/how-does-the-removesparseterms-in-r-work
tweetMatrix2 <- removeSparseTerms(tweetMatrix, .90)
tweetMatrix2

# Creating scale
tweetMatrix2Scale <- scale(tweetMatrix2)
tweetMatrix2Scale

# Distance Matrix
tweetMatrix2Dist <- dist(tweetMatrix2)

# Dendogram
# Font: https://dendrolab.wordpress.com/2010/11/03/construindo-dendrogramas-usando-o-r/
tweetMatrix2Hclust <- hclust(tweetMatrix2Dist)

# Creating dendograma (verify how words clustering each other)
plot(tweetMatrix2Hclust)

# Checking groups
cutree(tweetMatrix2Hclust, k = 3)

# Visualizing the word groups on dendogram
# Font: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/rect.hclust.html
rect.hclust(tweetMatrix2Hclust, k = 3, border = "red")


## Step 5 - Sentiment Analysis

# Load syuzhet package
library(syuzhet)

# Getting sentiment score for each tweet
# Font: http://dataaspirant.com/2018/03/22/twitter-sentiment-analysis-using-r/
tweetlistVector <- as.vector(tweetlist)
emotion <- get_nrc_sentiment(tweetlistVector)
emotion2 <- cbind(tweetlist, emotion)
head(emotion2)

# get_sentiment function to extract sentiment score for each of the tweets.
sentimentValue <- get_sentiment(tweetlistVector)

mostPositive <- tweetlistVector[sentimentValue == max(sentimentValue)]

mostPositive

# Segregating positive and negative tweets
# Positive Tweets
positiveTweets <- tweetlistVector[sentimentValue > 0]

head(positiveTweets)

# Negative Tweets
negativeTweets <- tweetlistVector[sentimentValue < 0]

head(negativeTweets)

# Neutral Tweets
neutralTweets <- tweetlistVector[sentimentValue == 0]

head(neutralTweets)

# Alternate way to classify as Positive, Negative or Neutral tweets
categorySentiment <- ifelse(sentimentValue < 0, "Negative", ifelse(sentimentValue > 0, "Positive", "Neutral"))

head(categorySentiment)

categorySentiment2 <- cbind(tweetlistVector, categorySentiment)

head(categorySentiment2)

# Tabule information
table(categorySentiment)



library(stringr)
library(plyr)

sentimento.score = function(sentences, pos.words, neg.words, .progress = 'none')
{
  
  # Criando um array de scores com lapply
  scores = laply(sentences,
                 function(sentence, pos.words, neg.words)
                 {
                   sentence = gsub("[[:punct:]]", "", sentence)
                   sentence = gsub("[[:cntrl:]]", "", sentence)
                   sentence =gsub('\\d+', '', sentence)
                   tryTolower = function(x)
                   {
                     y = NA
                     
                     # Tratamento de Erro
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     return(y)
                   }
                   
                   sentence = sapply(sentence, tryTolower)
                   word.list = str_split(sentence, "\\s+")
                   words = unlist(word.list)
                   pos.matches = match(words, pos.words)
                   neg.matches = match(words, neg.words)
                   pos.matches = !is.na(pos.matches)
                   neg.matches = !is.na(neg.matches)
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, pos.words, neg.words, .progress = .progress )
  
  scores.df = data.frame(text = sentences, score = scores)
  return(scores.df)
}

# Mapeando as palavras positivas e negativas
pos = readLines("pala")
neg = readLines("palavras_negativas.txt")

