# Use gsub instead of str_replace because str_replace replace just the first match.
# Function to clean tweets.
limpaTweets <- function(tweet){

    # Font: http://www.endmemo.com/program/R/grep.php
    # Remove http links
    tweet = gsub("(https?://*.[^\\s]+)", "", tweet)

    # Remove retweets
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)

    # Remove “#Hashtag”
    tweet = gsub("(#\\w*.[^\\s]+)", "", tweet)

    # Remove username “@people”
    tweet = gsub("(@\\w[^\\s]+)", "", tweet)

    # Remove punctuation
    tweet = gsub("(\\W)", " ", tweet)

    # Remove numbers
    tweet = gsub("(\\d)", "", tweet)

    # Remove unnecessary blank space
    tweet = gsub("\\s+", " ", str_trim(tweet))

    # Convertendo encoding de caracteres e convertendo para letra minúscula
    tweet <- stringi::stri_trans_general(tweet, "latin-ascii")
    tweet <- tolower(tweet)
    tweet <- iconv(tweet, from = "UTF-8", to = "ASCII")
}

# Remove http links
removeLink <- function(string){
    gsub("(https?://*.[^\\s]+)", "", string)
}

# Remove “#Hashtag”
removeHashtag <- function(string){
    gsub("(#\\w*.[^\\s]+)", "", string)
}

# Remove username “@string”
removeAtString <- function(string){
    gsub("(@\\w[^\\s]+)", "", string)
}

# Remove punctuation
removePunctuation <- function(string){
    gsub("(\\W)", " ", string)
}

# Remove numbers
removeNumber <- function(string){
    gsub("(\\d)", "", string)
}

# Remove unnecessary blank space
removeSpace <- function(string){
    gsub("\\s+", " ", str_trim(string))
}

# Convertendo encoding de caracteres e convertendo para letra minúscula
encodingLower <- function(string){
  string <- stringi::stri_trans_general(string, "latin-ascii")
  string <- tryTolower(string)
  string <- iconv(string, from = "UTF-8", to = "ASCII")
}

removeRT <- function(string){
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
}

# Function to cleanup tweet data
# Font: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf
limpaCorpus <- function(myCorpus){
  library(tm)
  myCorpus <- tm_map(myCorpus, tolower)
  # Remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # Remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
}