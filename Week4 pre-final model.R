# loading libraries

library(tm)
library(data.table)
library(quanteda)
library(tidyr)
library(stringr)
library(stringi)

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


# Cleaning the data

# data sampling

set.seed(123)
sampledData <- c(sample(blogs, length(blogs) * 0.05),
                 sample(news, length(news) * 0.05),
                 sample(twitter, length(twitter) * 0.05))


rm(blogs,news, twitter)

# data cleaning

# removing non-English characters
sampledMod <- iconv(sampledData, "latin1", "ASCII", sub="")

# creating and cleaning the corpus
corpus <- VCorpus(VectorSource(sampledMod))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


# create quanteda corpus
corp <- quanteda::corpus(corpus)

# tokenization
system.time(sparsedUnigram <- dfm(tokens(corp, ngrams = 1)))
system.time(sparsedBigram <- dfm(tokens(corp, ngrams = 2)))
system.time(sparsedTrigram <- dfm(tokens(corp, ngrams = 3)))
system.time(sparsed4gram <- dfm(tokens(corp, ngrams = 4)))

# reduce sparsity by removing non-frequent terms
Unigrams <- dfm_trim(sparsedUnigram, min_termfreq = 2)
Bigrams <- dfm_trim(sparsedBigram, min_termfreq = 2)
Trigrams <- dfm_trim(sparsedTrigram, min_termfreq = 2)
Fourgrams <- dfm_trim(sparsed4gram, min_termfreq = 2)

# frequency ordering
frequencyFrame <- function(dfm){
    freq <- sort(colSums(dfm), decreasing=TRUE)
    frequencyFrame <- data.table(word=names(freq), freq=freq)
    return(frequencyFrame)
}

UnigramsSorted <- frequencyFrame(Unigrams)
BigramsSorted <- frequencyFrame(Bigrams)
TrigramsSorted <- frequencyFrame(Trigrams)
FourgramsSorted <- frequencyFrame(Fourgrams)

# remove _ from word column

BigramsSorted$word <- gsub("_"," ",BigramsSorted$word)
TrigramsSorted$word <- gsub("_"," ",TrigramsSorted$word)
FourgramsSorted$word <- gsub("_"," ",FourgramsSorted$word)

saveRDS(UnigramsSorted , "./data/Unigrams.rds")
saveRDS(BigramsSorted , "./data/Bigrams.rds")
saveRDS(TrigramsSorted , "./data/Trigrams.rds")
saveRDS(FourgramsSorted , "./data/Fourgrams.rds")



# next word prediction

nextWordPred <- function(sentence){
    
    # clean the input sentence
    words <- tolower(sentence) 
    words <- gsub("[[:punct:]]","", words ) #remove punctuation
    words <- gsub("\\d", "", words)  #reomve numbers
    words <- str_trim(words) #remove unnecessary spaces
    words <- unlist(strsplit(words, split = " ")) # split the sentences into words
    
    if(length(words)>1){
        words4gram <- paste(words[(length(words)-2):length(words)], collapse = " ") # select last 3 words 
        
        # Output the most frequent 4 gram predicted word
        Predicted4gram <- stri_extract_last_words(FourgramsSorted[word %like% paste0("^",words4gram)][1]$word)
        
        if(is.na(Predicted4gram) == FALSE){
            return(Predicted4gram)
            
        } else if(is.na(Predicted4gram) == TRUE) {
            
            words3gram <- paste(words[(length(words)-1):length(words)], collapse = " ") # select last 2 words
            
            # Output the most frequent Trigram predicted word
            PredictedTrigram <- stri_extract_last_words(TrigramsSorted[word %like% paste0("^",words3gram)][1]$word)
            
            if (is.na(PredictedTrigram) == FALSE){
                return(PredictedTrigram)   
                
            } else if(is.na(PredictedTrigram) == TRUE) {
                
                words2gram <- paste(words[length(words)], collapse = " ") # select last word
                
                # Output the most frequent Bigram predicted word    
                PredictedBigram <- stri_extract_last_words(BigramsSorted[word %like% paste0("^",words2gram)][1]$word)   
                
                if (is.na(PredictedBigram) == FALSE){
                    return(PredictedBigram)    
                } else {
                    return("the")   # return the most frequent Unigram
                } 
            }
        }
    } else {
        return("the") 
    }
}