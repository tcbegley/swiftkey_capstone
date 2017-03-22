library(tm)
library(dplyr)
library(ggplot2)

### download data ###
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

if(!file.exists("Coursera-Swiftkey.zip")){
    download.file(url, "Coursera-Swiftkey.zip", method="curl")
}
if(!file.exists("final")) {
    files <- c("final/en_US/en_US.blogs.txt", "final/en_US/en_US.news.txt", "final/en_US/en_US.twitter.txt")
    unzip("Coursera-SwiftKey.zip", files=files)
}

if(!file.exists("data.RDS")) {
    ### load data ###
    en_blog <- readLines("final/en_US/en_US.blogs.txt", skipNul = TRUE)
    en_news <- readLines("final/en_US/en_US.news.txt", skipNul = TRUE)
    en_twitter <- readLines("final/en_US/en_US.twitter.txt", skipNul = TRUE)
    
    ### basic stats ###
    blog_size <- file.size("final/en_US/en_US.blogs.txt")
    news_size <- file.size("final/en_US/en_US.news.txt")
    twitter_size <- file.size("final/en_US/en_US.twitter.txt")
    sizes <- c(blog_size, news_size, twitter_size)
    
    blog_length <- length(en_blog)
    news_length <- length(en_news)
    twitter_length <- length(en_twitter)
    lengths <- c(blog_length, news_length, twitter_length)
    
    blog_words <- sum(sapply(gregexpr("[[:alpha:]]+", en_blog), function(x) sum(x > 0)))
    news_words <- sum(sapply(gregexpr("[[:alpha:]]+", en_news), function(x) sum(x > 0)))
    twitter_words <- sum(sapply(gregexpr("[[:alpha:]]+", en_twitter), function(x) sum(x > 0)))
    words <- c(blog_words, news_words, twitter_words)
    
    ### collect basic stats ###
    stats <- data.frame(Name = c("en_US.blogs", "en_US.news", "en_US.twitter"),
                        Size_MB = c(blog_size, news_size, twitter_size)/2^20,
                        Length = c(blog_length, news_length, twitter_length),
                        Words = c(blog_words, news_words, twitter_words))
    stats <- mutate(stats, Mean_words = Words/Length)
    
    ### extract data sample ###
    set.seed(3333)
    data <- c(sample(en_blog, blog_length/50), 
              sample(en_news, news_length/50), 
              sample(en_twitter, twitter_length/50))
    
    ### clean sample ###
    data <- VCorpus(VectorSource(data))
    
    # convert to lower case
    data <- tm_map(data, content_transformer(tolower))
    
    #remove urls and twitter handles
    data <- tm_map(data, content_transformer(function(x) gsub("http[[:alnum:][:punct:]]*", "", x)))
    data <- tm_map(data, content_transformer(function(x) gsub("@[:alnum:]+", "", x)))
    
    #remove numbers and punctuation
    data <- tm_map(data, removePunctuation)
    data <- tm_map(data, removeNumbers)
    
    # eliminate extra whitespace
    data <- tm_map(data, stripWhitespace)
    
    saveRDS(data, "data.RDS")
}

if(!file.exists("unigrams.RDS") || !file.exists("bigrams.RDS") || 
   !file.exists("trigrams.RDS") || !file.exists("quadgrams.RDS")) {
    
    data <- readRDS("data.RDS")
    
    
    ### extract n-grams ###
    # function to extract bigrams
    bigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
    
    # analogous function for trigrams
    trigramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
    
    # analogous function for quadgrams
    quadgramTokenizer <-
        function(x)
            unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
    
    # create term document matrices and remove sparse terms for efficiency's sake
    tdm1 <- removeSparseTerms(TermDocumentMatrix(data), 0.9999)
    tdm2 <- removeSparseTerms(TermDocumentMatrix(data, control = list(tokenize = bigramTokenizer)), 0.9999)
    tdm3 <- removeSparseTerms(TermDocumentMatrix(data, control = list(tokenize = trigramTokenizer)), 0.9999)
    tdm4 <- removeSparseTerms(TermDocumentMatrix(data, control = list(tokenize = quadgramTokenizer)), 0.9999)
    
    # count occurences of n-grams for n = 1,2,3 and store in data frames
    unigrams <- sort(rowSums(as.matrix(tdm1)), decreasing=TRUE)
    unigrams <- data.frame(unigram = names(unigrams), freq = unigrams, stringsAsFactors = FALSE)
    row.names(unigrams) <- NULL
    bigrams <- sort(rowSums(as.matrix(tdm2)), decreasing=TRUE)
    bigrams <- data.frame(bigram = names(bigrams), freq = bigrams, stringsAsFactors = FALSE)
    row.names(bigrams) <- NULL
    trigrams <- sort(rowSums(as.matrix(tdm3)), decreasing=TRUE)
    trigrams <- data.frame(trigram = names(trigrams), freq = trigrams, stringsAsFactors = FALSE)
    row.names(trigrams) <- NULL
    quadgrams <- sort(rowSums(as.matrix(tdm4)), decreasing=TRUE)
    quadgrams <- data.frame(quadgram = names(quadgrams), freq = quadgrams, stringsAsFactors = FALSE)
    row.names(quadgrams) <- NULL
    
    
    #split n-grams into their constituent words
    bg_word1 <- sapply(strsplit(bigrams$bigram, split = " "), function(x) x[1])
    bg_word2 <- sapply(strsplit(bigrams$bigram, split = " "), function(x) x[2])
    tg_word1 <- sapply(strsplit(trigrams$trigram, split = " "), function(x) x[1])
    tg_word2 <- sapply(strsplit(trigrams$trigram, split = " "), function(x) x[2])
    tg_word3 <- sapply(strsplit(trigrams$trigram, split = " "), function(x) x[3])
    qg_word1 <- sapply(strsplit(quadgrams$quadgram, split = " "), function(x) x[1])
    qg_word2 <- sapply(strsplit(quadgrams$quadgram, split = " "), function(x) x[2])
    qg_word3 <- sapply(strsplit(quadgrams$quadgram, split = " "), function(x) x[3])
    qg_word4 <- sapply(strsplit(quadgrams$quadgram, split = " "), function(x) x[4])
    
    # add new columns to bigrams and trigrams to be used as observed-predictor pairs
    bigrams <- cbind(bigrams, bg_word1, bg_word2)
    trigrams <- cbind(trigrams, paste(tg_word1, tg_word2), tg_word3)
    quadgrams <- cbind(quadgrams, paste(qg_word1, qg_word2, qg_word3), qg_word4)
    
    names(bigrams) <- c("bigram", "freq", "obs", "pred")
    names(trigrams) <- c("trigram", "freq", "obs", "pred")
    names(quadgrams) <- c("quadgram", "freq", "obs", "pred")
    
    # for each n-gram, we compare all entries with the same observed (n-1)-gram, and 
    # calculate the proportion of times that the prediction was observed
    bg_grouped_obs <- group_by(bigrams, obs)
    tg_grouped_obs <- group_by(trigrams, obs)
    qg_grouped_obs <- group_by(quadgrams, obs)
    
    bg <- mutate(bg_grouped_obs, total=sum(freq)) 
    #bg <- filter(bg, freq == max(freq))
    tg <- mutate(tg_grouped_obs, total=sum(freq))
    #tg <- filter(tg, freq == max(freq))
    qg <- mutate(qg_grouped_obs, total=sum(freq))
    #qg <- filter(qg, freq == max(freq))
    
    bg$obs <- as.character(bg$obs)
    bg$pred <- as.character(bg$pred)
    tg$obs <- as.character(tg$obs)
    tg$pred <- as.character(tg$pred)
    qg$obs <- as.character(qg$obs)
    qg$pred <- as.character(qg$pred)
    
    
    # the new variable s stores the probability of seeing the prediction given the observation
    unigrams <- mutate(unigrams, s = freq / sum(unigrams$freq))
    bigrams <- mutate(bg, s = freq / total)
    trigrams <- mutate(tg, s = freq / total)
    quadgrams <- mutate(qg, s = freq / total)
    
    rm(bg)
    rm(tg)
    rm(qg)
    
    # save data frames for future use
    saveRDS(unigrams, "unigrams.RDS")
    saveRDS(bigrams, "bigrams.RDS")
    saveRDS(trigrams, "trigrams.RDS")
    saveRDS(quadgrams, "quadgrams.RDS")
}

message("Model ready for use!")


