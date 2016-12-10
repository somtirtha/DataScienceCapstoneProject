library(stringi)
## Warning: package 'stringi' was built under R version 3.2.5
library(tm)
## Loading required package: NLP
library(ggplot2)

load("RData/sample_blogs.RData")
load("RData/sample_news.RData")
load("RData/sample_twitter.RData")

# Disk size (in MB)
blogs_dsize <- file.info("RData/sample_blogs.RData")$size / 1024 / 1024
news_dsize <- file.info("RData/sample_news.RData")$size / 1024 / 1024
twitter_dsize <- file.info("RData/sample_twitter.RData")$size / 1024 / 1024

#In-memory size (in MB)
blogs_msize<-object.size(sample_blogs) / 1024 / 1024
news_msize<-object.size(sample_news) / 1024 / 1024
twitter_msize<-object.size(sample_twitter) / 1024 / 1024

# Words in lines
blogs_words <- stri_count_words(sample_blogs)
news_words <- stri_count_words(sample_news)
twitter_words <- stri_count_words(sample_twitter)

# Summary
data.frame(source = c("sample_blogs", "sample_news", "sample_twitter"),
           files_MB = c(blogs_dsize, news_dsize, twitter_dsize),
           in_memory_MB = c(blogs_msize, news_msize, twitter_msize),
           lines = c(length(sample_blogs), length(sample_news), length(sample_twitter)),
           words_num = c(sum(blogs_words), sum(news_words), sum(twitter_words)),
           mean_words_num = c(mean(blogs_words), mean(news_words), mean(twitter_words)))


# this file contains limited data - head(..., 5000)
# load("~/CourseraR/Capstone/e2.RData")
data<-c(sample_blogs,sample_news,sample_twitter)

#Corpus object for tm_map functions
vector_doc <- VectorSource(data)
corpus <- VCorpus(vector_doc)

corpus <- tm_map(corpus,  content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),  mc.cores=1)

toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x,fixed=TRUE))


corpus<-tm_map(corpus, content_transformer(tolower))
corpus<-tm_map(corpus, stripWhitespace)

#Removing...
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus, removeNumbers)
corpus<-tm_map(corpus, removeWords, stopwords('english'))

#RWeka controls failed to for me with strage instant error, so I found another tokenizers:
BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
TrigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)


#Count words
freq_df <- function(tdm){
    freq <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    freq_df <- data.frame(word=names(freq), freq=freq)
    return(freq_df)
}

unigram <- removeSparseTerms(TermDocumentMatrix(corpus), 0.9999)
unigram_freq <- freq_df(unigram)

bigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer)), 0.9999)
bigram_freq <- freq_df(bigram)

trigram <- removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = TrigramTokenizer)), 0.9999)
trigram_freq <- freq_df(trigram)

freq_plot <- function(data, title) {
    ggplot(data[1:25,], aes(reorder(word, -freq), freq)) +
        labs(x = "Words/Phrases", y = "Frequency") +
        ggtitle(title) +
        theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
        geom_bar(stat = "identity")
}

freq_plot(unigram_freq, "Top-25 Unigrams")





