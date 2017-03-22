#   Implements the 'stupid backoff' algorithm for prediction
#
#   Read details here: http://www.aclweb.org/anthology/D07-1090


library(tm)
library(stringr)
library(dplyr)

unigrams <- readRDS("unigrams.RDS")
bigrams <- readRDS("bigrams.RDS")
trigrams <- readRDS("trigrams.RDS")
quadgrams <- readRDS("quadgrams.RDS")


clean_input <- function(text) {
    # convert to lower case
    text <- tolower(text)
    
    #remove urls and twitter handles
    text <- gsub("http[[:alnum:][:punct:]]*", "", text)
    text <- gsub("@[:alnum:]+", "", text)
    
    #remove numbers and punctuation
    text <- removePunctuation(text)
    text <- removeNumbers(text)
    
    # eliminate extra whitespace
    text <- stripWhitespace(text)
    text <- str_trim(text)
    
    return(text)
}

next_word <- function(text="") {
    # clean input and split into character vector
    # we reverse the result for easy indexing of the most recently typed words
    words <- rev(unlist(strsplit(clean_input(text), split=" ")))
    
    last <- words[1]
    last2 <- paste(words[2], words[1], sep=" ")
    last3 <- paste(words[3], words[2], words[1], sep=" ")
    
    # extract all matches from n-grams
    match1 <- bigrams$obs == last
    match2 <- trigrams$obs == last2
    match3 <- quadgrams$obs == last3
    pred1 <- unigrams$unigram
    pred2 <- bigrams$pred[match1]
    pred3 <- trigrams$pred[match2]
    pred4 <- quadgrams$pred[match3]
    
    preds <- c(pred1, pred2, pred3, pred4)
    
    # weight likelihoods to favour longer n-grams, 0.4 is number used in stupid backoff
    s1 <- 0.4*0.4*0.4*unigrams$s
    s2 <- 0.4*0.4*bigrams$s[match1]
    s3 <- 0.4*trigrams$s[match2]
    s4 <- quadgrams$s[match3]
    
    s <- c(s1, s2, s3, s4)
    
    preds <- data.frame(cbind(preds, s), stringsAsFactors = FALSE)
    preds$s <- as.numeric(preds$s)
    
    # if the same word is predicted by n-grams for different n, then combine them
    pred_group <- group_by(preds, preds)
    preds <- summarise(pred_group, s = sum(s))
    
    # order predictions by likelihood
    preds <- preds[order(preds$s, decreasing=TRUE),]
    
    # return top 3
    return(head(preds, 3))
}