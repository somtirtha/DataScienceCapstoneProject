library(R.utils)
library(SnowballC)
library(tm)
library(qdap)

tweets = iconv(readLines("../final/en_US/en_US.twitter.txt", 10000), to="utf-8-mac", sub="") # used utf-8-mac for mac else use utf-8

# cleaning the data
cleanCorpus = function(text) {
    text = (text[!is.na(text)])
    all_text = text
    all_text = sent_detect(all_text, language = "en", model = NULL)

    # filtering the data
    all_text_corpus = VCorpus(VectorSource(all_text))
    all_text_corpus = tm_map(all_text_corpus, stripWhitespace)
    all_text_corpus = tm_map(all_text_corpus, removePunctuation)
    all_text_corpus = tm_map(all_text_corpus, content_transformer(tolower))
    all_text_corpus = tm_map(all_text_corpus, removeNumbers)

    all_text_corpus <- tm_map(all_text_corpus, removeWords, stopwords("english"))
    all_text_corpus = tm_map(all_text_corpus, stemDocument)
    all_text_corpus = tm_map(all_text_corpus, PlainTextDocument)

    all_text_corpus
}

# call cleanCorpus
clean_corpus = cleanCorpus(tweets)

# exploring the data
# create a document term matrix
dtm <- DocumentTermMatrix(clean_corpus)

# create a term document matrix
tdm <- TermDocumentMatrix(clean_corpus)

freq <- colSums(as.matrix(dtm))
length(freq)
ord_freq <- order(freq)
freq[head(ord_freq)]
freq[tail(ord_freq)]

head(table(freq), 20)
tail(table(freq), 20)

# remove sparse terms
dtms <- removeSparseTerms(dtm, 0.1)
inspect(dtms)

freq <- colSums(as.matrix(dtms))
freq

# Creating teh N-Gram model
# n-gram model
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(clean_corpus, control = list(tokenize = BigramTokenizer))

# unigram <- content_transformer ( ngram_tokenizer ( 1 ) )
# bigram <- content_transformer ( ngram_tokenizer ( 2 ) )
# trigram <- content_transformer ( ngram_tokenizer ( 3 ) )
#
# bigram_model = TermDocumentMatrix ( clean_corpus, control=list (tokenize=bigram) )
# barchart ( ??, main="Bi-Gram Word Frequency Count from Blogs", ylab="Words", xlab="Frequency", col="green" )


# tokenizer function
#' Ngrams tokenizer
#'
#' @param n integer
#' @param skip_word_none boolean see: ?stri_split_boundaries
#' @param skip_word_number boolean see: ?stri_split_boundaries
#' @return n-gram tokenizer function
#' @examples
#' trigram_tokenizer <- ngram_tokenizer(3)
#' trigram_tokenizer(as.character(citation()))
#'
ngram_tokenizer <- function(n = 1L, skip_word_none = TRUE, skip_word_number = FALSE) {
    stopifnot(is.numeric(n), is.finite(n), n > 0)

    #' To avoid :: calls
    stri_split_boundaries <- stringi::stri_split_boundaries
    stri_join <- stringi::stri_join

    options <- stringi::stri_opts_brkiter(
        type="word", skip_word_none = skip_word_none, skip_word_number = skip_word_number
    )

    #' Tokenizer
    #'
    #' @param x character
    #' @return character vector with n-grams
    function(x) {
        stopifnot(is.character(x))

        # Split into word tokens
        tokens <- unlist(stri_split_boundaries(x, opts_brkiter=options))
        len <- length(tokens)

        if(all(is.na(tokens)) || len < n) {
            # If we didn't detect any words or number of tokens is less than n return empty vector
            character(0)
        } else {
            sapply(
                1:max(1, len - n + 1),
                function(i) stri_join(tokens[i:min(len, i + n - 1)], collapse = " ")
            )
        }
    }
}
