# load the stingi library for text manipulation
library(stringi)

# set curret working directory
setwd('/Users/somtirtha/workspace/Programs/Coursera/DataScience/DataScienceCapstoneProject/Week2/')

# read data
# docs = Corpus(DirSource("../final/en_US/en_US.twitter.txt"))
# summary(docs)

# import the blogs and twitter datasets in text mode
en_blogs <- readLines("../final/en_US/en_US.blogs.txt", encoding="UTF-8")
en_twitter <- readLines("../final/en_US/en_US.twitter.txt", encoding="UTF-8")

# import the news dataset in binary mode
con <- file("../final/en_US/en_US.news.txt", open="rb")
en_news <- readLines(con, encoding="UTF-8")
close(con)
rm(con)

# drop non UTF-8 characters from twitter data
en_twitter <- iconv(en_twitter, from = "latin1", to = "UTF-8", sub="")
en_twitter <- stri_replace_all_regex(en_twitter, "\u2019|`","'")
en_twitter <- stri_replace_all_regex(en_twitter, "\u201c|\u201d|u201f|``",'"')


# save the data to .RData files for easy access
save(en_blogs, file="en_blogs.RData")
save(en_news, file="en_news.RData")
save(en_twitter, file="en_twitter.RData")