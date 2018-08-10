# loading libraries

library(tm)
library(stringi)
library(SnowballC)
library(RWeka)
library(ggplot2)

# Getting the data

setwd("E:/Data science/JHU Coursera/Course 10 Capstone")
if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file(
        "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
        "Coursera-SwiftKey.zip"
    )
    unzip("Coursera-SwiftKey.zip")
}
blogs <-
    readLines("final/en_US/en_US.blogs.txt",
              skipNul = TRUE,
              encoding = "UTF-8")
news <-
    readLines(
        "final/en_US/en_US.news.txt",
        skipNul = TRUE,
        warn = FALSE,
        encoding = "UTF-8"
    )
twitter <-
    readLines("final/en_US/en_US.twitter.txt",
              skipNul = TRUE,
              encoding = "UTF-8")

# Data summary

# Get file sizes
blogs.size <-
    file.info("final/en_US/en_US.blogs.txt")$size / (1024 * 1024)
news.size <-
    file.info("final/en_US/en_US.news.txt")$size / (1024 * 1024)
twitter.size <-
    file.info("final/en_US/en_US.twitter.txt")$size / (1024 * 1024)

# Number of words in a file
blogs.words <- stri_count_words(blogs)
news.words <- stri_count_words(news)
twitter.words <- stri_count_words(twitter)

# Summary data frame
SummaryDF <-
    data.frame(
        file.name = c("blogs", "news", "twitter"),
        size.inMB = c(blogs.size, news.size, twitter.size),
        lines.number = c(length(blogs), length(news), length(twitter)),
        words.number = c(sum(blogs.words), sum(news.words), sum(twitter.words)),
        Avg.words.number = c(mean(blogs.words), mean(news.words), mean(twitter.words)),   
        Max.wordsPerLine = c(max(nchar(blogs)),max(nchar(news)),max(nchar(twitter)))
    )

SummaryDF

# Cleaning the data

# data sampling

set.seed(123)
sampledData <- c(sample(blogs, length(blogs) * 0.01),
                 sample(news, length(news) * 0.01),
                 sample(twitter, length(twitter) * 0.01))

# data cleaning

# removing non-English characters
sampledMod <- iconv(sampledData, "latin1", "ASCII", sub="")

# creating and cleaning the corpus
corpus <- VCorpus(VectorSource(sampledMod))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)


# remove profanity using the google badwords database.

profanityWords <- readLines("google_bad_words.txt", skipNul = TRUE)
corpus <- tm_map(corpus, removeWords, profanityWords)

#N-grams

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

sparsedUnigram <- TermDocumentMatrix(corpus, control = list(tokenize = UnigramTokenizer))
sparsedBigram <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))
sparsedTrigram <- TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer))

sparsedUnigram
sparsedBigram
sparsedTrigram

# remove sparsity

Unigrams <- removeSparseTerms(sparsedUnigram, 0.99)
Bigrams <- removeSparseTerms(sparsedBigram, 0.9999)
Trigrams <- removeSparseTerms(sparsedTrigram, 0.9999)

frequencyFrame <- function(tdm){
    freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    frequencyFrame <- data.frame(word=names(freq), freq=freq)
    return(frequencyFrame)
}


UnigramsSorted <- frequencyFrame(Unigrams)
BigramsSorted <- frequencyFrame(Bigrams)
TrigramsSorted <- frequencyFrame(Trigrams)


# Unigram plot

g <- ggplot(data = UnigramsSorted[1:20,], aes(x = reorder(word,-freq), y= freq)) + geom_bar(stat="identity")
g <- g + labs(x= "Unigram", y = "Frequency", title = "Top 20 frequent unigrams")
g <- g + theme(axis.text.x=element_text(angle=90))
g


# Bigram plot

g <- ggplot(data = BigramsSorted[1:20,], aes(x = reorder(word,-freq), y= freq)) + geom_bar(stat="identity")
g <- g + labs(x= "Bigram", y = "Frequency", title = "Top 20 frequent Bigrams")
g <- g + theme(axis.text.x=element_text(angle=90))
g


# Trigram plot

g <- ggplot(data = TrigramsSorted[1:20,], aes(x = reorder(word,-freq), y= freq)) + geom_bar(stat="identity")
g <- g + labs(x= "Trigram", y = "Frequency", title = "Top 20 frequent Trigrams")
g <- g + theme(axis.text.x=element_text(angle=90))
g

