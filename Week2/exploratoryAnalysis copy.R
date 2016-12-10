library(R.utils)
library(SnowballC)
library(tm)
library(RWeka)
library(openNLP)
library(qdap)
library(ggplot2)
library(stringi)

# set curret working directory
setwd('/Users/somtirtha/workspace/Programs/Coursera/DataScience/DataScienceCapstoneProject/Week2/')
# options(java.parameters = "-Xmx24000m")

# inputFile = "../final/en_US/en_US.blogs.txt"
# inputFile = "../sub_dataset.txt"
tweets = iconv(readLines("../final/en_US/en_US.twitter.txt", 1000), to="utf-8-mac") # used utf-8-mac for mac else use utf-8
tweets = (tweets[!is.na(tweets)])
# blogs  = readLines("../final/en_US/en_US.blogs.txt", 5000)
# news   = readLines("../final/en_US/en_US.news.txt", 5000)

# all_text = paste(tweets, blogs, news)
all_text = tweets
all_text = sent_detect(all_text, language = "en", model = NULL)

# cleaning the data
all_text_corpus = VCorpus(VectorSource(all_text))
all_text_corpus = tm_map(all_text_corpus, stripWhitespace)
all_text_corpus = tm_map(all_text_corpus, removePunctuation)
all_text_corpus = tm_map(all_text_corpus, content_transformer(tolower))
all_text_corpus = tm_map(all_text_corpus, removeNumbers)
# all_text_corpus = Corpus(VectorSource(all_text_corpus))

# profanity filtering
profanity_file  = 'profanity_list.txt'
profanity_text  = readLines(profanity_file) # VectorSource(readLines(profanity_file))
all_text_corpus = tm_map(all_text_corpus, removeWords, profanity_text, lazy=TRUE)

# tokenization
# all_text_df = data.frame(text=unlist(sapply(all_text_corpus, '[',"content")),stringsAsFactors=F)

delimiters = " \\t\\r\\n.!?,;\"()"
# one_gram_tokenizer   = function(x) NGramTokenizer(x, Weka_control(min=1,max=1))
# 
# two_gram   = NGramTokenizer(all_text_df, Weka_control(min=2,max=2, delimiters = delimiters))
# three_gram = NGramTokenizer(all_text_df, Weka_control(min=3,max=3, delimiters = delimiters))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm <- TermDocumentMatrix(all_text_corpus, control = list(tokenize = BigramTokenizer))

one_word = data.frame(table(one_gram))
two_word = data.frame(table(two_gram))
three_word = data.frame(table(three_gram))

sorted_one = one_word[order(one_word$Freq,decreasing=TRUE),]
colnames(sort_one) = c("Word", "Freq")
sort_two = two_word[order(two_word$Freq,decreasing=TRUE),]
colnames(sort_two) = c("Word", "Freq")
sort_three = three_word[order(three_word$Freq,decreasing=TRUE),]
colnames(sort_three) = c("Word", "Freq")


#

# file.info("final/en_US/en_US.blogs.txt")$size   / 1024^2
# 
# stri_stats_general(lines)
# 
# #
# blogs = lines
# words_blogs   = stri_count_words(blogs)
# summary( words_blogs )
# qplot(   words_blogs )







