
# loading libraries

library(tm)
library(data.table)
library(quanteda)
library(tidyr)
library(stringr)
library(stringi)

# read N-grams

UnigramsSorted <- readRDS("data/Unigrams.rds")
BigramsSorted <- readRDS("data/Bigrams.rds")
TrigramsSorted <- readRDS("data/Trigrams.rds")
FourgramsSorted <- readRDS("data/Fourgrams.rds")


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
