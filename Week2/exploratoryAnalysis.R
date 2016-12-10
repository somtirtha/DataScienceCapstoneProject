library(R.utils)
library(SnowballC)
library(tm)
library(RWeka)
library(dplyr)
library(qdap)
# load the magrittr package for piping
library(magrittr) 
# load the stingi library for text manipulation
library(stringi)

# set current working directory
setwd('/Users/somtirtha/workspace/Programs/Coursera/DataScience/DataScienceCapstoneProject/Week2/')

# load saved data
load("RData/en_blogs.RData")
load("RData/en_news.RData")
load("RData/en_twitter.RData")

# sample data (100,000 of each)
sample_blogs   <- sample(en_blogs, 1000)
sample_news    <- sample(en_news, 1000)
sample_twitter <- sample(en_twitter, 1000)

# clean the data
cleanCorpus = function(text) {
    text = (text[!is.na(text)])
    all_text = text
    # all_text = sent_detect(all_text, language = "en", model = NULL)
    
    # filtering the data
    all_text_corpus = VCorpus(VectorSource(all_text))
    all_text_corpus = tm_map(all_text_corpus, stripWhitespace)
    all_text_corpus = tm_map(all_text_corpus, removePunctuation)
    all_text_corpus = tm_map(all_text_corpus, content_transformer(tolower))
    all_text_corpus = tm_map(all_text_corpus, removeNumbers)

    all_text_corpus <- tm_map(all_text_corpus, removeWords, stopwords("english"))
    all_text_corpus = tm_map(all_text_corpus, stemDocument)
    all_text_corpus = tm_map(all_text_corpus, PlainTextDocument)
    
    # all_text
    all_text_corpus
}


sample_blogs = cleanCorpus(sample_blogs)
sample_news = cleanCorpus(sample_news)
sample_twitter = cleanCorpus(sample_twitter)

# saved the cleaned data
save(sample_blogs, file="RData/sample_blogs.RData")
save(sample_news, file="RData/sample_news.RData")
save(sample_twitter, file="RData/sample_twitter.RData")

# save samples
save(sample_blogs, sample_news, sample_twitter, file= "RData/sample_data.RData")

# summarizing data

# file size (in MegaBytes/MB)
file.info("../final/en_US/en_US.blogs.txt")$size   / 1024^2
file.info("../final/en_US/en_US.news.txt")$size    / 1024^2
file.info("../final/en_US/en_US.twitter.txt")$size / 1024^2

# number of lines
length(en_blogs)
length(en_news)
length(en_twitter)

# number of characters per line
summary( nchar(en_blogs)   )
summary( nchar(en_news)    )
summary( nchar(en_twitter) )

# more character analysis analysis
stats_blogs   <- stri_stats_general(en_blogs)
stats_blogs
stats_news    <- stri_stats_general(en_news)
stats_news
stats_twitter <- stri_stats_general(en_twitter)
stats_twitter

# textual analysis
words_blogs   <- stri_count_words(en_blogs)
words_news    <- stri_count_words(en_news)
words_twitter <- stri_count_words(en_twitter)

# summaries
summary( words_blogs   )
summary( words_news    )
summary( words_twitter )

# plots
library(ggplot2)
qplot(words_blogs, xlim = c(0, 200), binwidth=10)
qplot(words_news, xlim = c(0, 200), binwidth=5)
qplot(words_twitter, xlim = c(0, 50), binwidth=5)


#==========================
# construct frequency table

# load the sample data
# load("sample_data.RData")
# summary(sample_data)
# sample(sample_data, 1000)
# load("RData/sample_blogs.RData")
load("RData/en_twitter.RData")
summary(sample_blogs)
# summary(en_twitter)

vector_doc <- VectorSource(sample_blogs)
# vector_doc <- VectorSource(en_twitter)
corpus <- VCorpus(vector_doc)

TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


freq_df <- function(tdm){
    freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    freq_df <- data.frame(word=names(freq), freq=freq)
    return(freq_df)
}

trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)









# ngram tokaniser
n <- 2L
bigram_token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
n <- 3L
trigram_token <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))

# check length function
length_is <- function(n) function(x) length(x)==n

# contruct single corpus from sample data
vc_blogs <- sample_blogs %>% 
    data.frame() %>% 
    DataframeSource() %>%
    VCorpus %>%
    tm_map( stripWhitespace )

# vc_news <-
#     sample_news %>%
#     data.frame() %>%
#     DataframeSource() %>%
#     VCorpus %>%
#     tm_map( stripWhitespace )
# 
# vc_twitter <-
#     sample_twitter %>%
#     data.frame() %>%
#     DataframeSource() %>%
#     VCorpus %>%
#     tm_map( stripWhitespace )

# vc_all <- c(vc_blogs, vc_news, vc_twitter)
vc_all <- c(vc_blogs)

# frequency unigrams
tdm_unigram <-
    vc_all %>%
    TermDocumentMatrix( control = list( removePunctuation = TRUE,
                                        removeNumbers = TRUE,
                                        wordLengths = c( 1, Inf) )
    )

freq_unigram <- 
    tdm_unigram %>%
    as.matrix %>%
    rowSums

# write all unigrams to a list
# in order to create uniform levels of factors
unigram_levels <- unique(tdm_unigram$dimnames$Terms)

# trigram Term-Document Matrix
tdm_trigram <-
    vc_all %>%
    TermDocumentMatrix( control = list( removePunctuation = TRUE,
                                        removeNumbers = TRUE,
                                        wordLengths = c( 1, Inf),
                                        tokenize = trigram_token)
    )

# aggregate frequencies
tdm_trigram %>%
    as.matrix %>%
    rowSums -> freq_trigram

# repeat by frequency
freq_trigram %<>%
    names %>%
    rep( times = freq_trigram )

# split the trigram into three columns
freq_trigram %<>%
    strsplit(split=" ")

# filter out those of less than three columns
freq_trigram <- do.call(rbind, 
                        Filter( length_is(3),
                                freq_trigram )
)

# transform to data.frame encode as factorssomtirtha

df_trigram <- data.frame(X1 = factor(freq_trigram[,1], levels = unigram_levels),
                         X2 = factor(freq_trigram[,2], levels = unigram_levels),
                         Y  = factor(freq_trigram[,3], levels = unigram_levels) )
# save data frame
save( df_trigram, unigram_levels, file = "df_trigram.RData")











