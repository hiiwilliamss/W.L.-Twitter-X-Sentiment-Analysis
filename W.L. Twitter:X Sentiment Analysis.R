###############################
# W.L.
# 07/27/2024
# Twitter/X Sentiment Analysis
###############################

# Checking file
apple <- read.csv(file.choose(), header = T)
str(apple)

# Construct Corpus
library(tm)
corpus <- iconv(apple$text, to = "utf-8-mac")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# Data cleaning
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords("english"))
inspect(cleanset[1:5])

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset <- tm_map(cleanset, removeWords, c("aapl", "apple"))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = "stocks",
                   replacement = "stock")

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# Term Document Matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Bar Plot
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w, 
        las = 2,
        col = rainbow(50))

# Wordcloud
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = T)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, "PuRd"),
          scale = c(5, 0.3),
          rot.per = 0.7)

# Wordcloud 2
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c("word", "freq")
wordcloud2(w,
           size = 0.8,
           shape = "circle")

# Sentiment Analysis of Tweets
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# Read file
apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to = "utf-8-mac")

# Sentiment scores
s <- get_nrc_sentiment(tweets)
head(s)

tweets[4]
get_nrc_sentiment("ugly")

# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = "Count",
        main = "Sentiment Scores for Apple Tweets")
