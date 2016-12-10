
library(R.utils)

# set working directory
setwd('/Users/somtirtha/workspace/Programs/Coursera/DataScience/DataScienceCapstoneProject/Week1/')
# setwd(getwd())

#Q1
file.info("Coursera-Swiftkey/final/en_US/en_US.blogs.txt")$size / 1024^2

#Q2

blogs <- readLines("Coursera-Swiftkey/final/en_US/en_US.blogs.txt")
news <- readLines("Coursera-Swiftkey/final/en_US/en_US.news.txt")
twitter <- readLines("Coursera-Swiftkey/final/en_US/en_US.twitter.txt")
length(twitter)

#3
inputFile <- "../final/en_US/en_US.blogs.txt"
# inputFile <- "sub_dataset.txt"
lines <- readLines(inputFile)

max_char = -1
for (i in lines) {
    if ( nchar(i) > max_char ) {
        max_char = nchar(i)
    }
}

print(max_char)

# OR the following way
# max(nchar(blogs))
# max(nchar(news))
# max(nchar(twitter))

#4
inputFile <- "../final/en_US/en_US.twitter.txt"
lines <- readLines(inputFile)
love_count <- length(grep("\\<love\\>", lines))
hate_count <- length(grep("\\<hate\\>", lines))

print(love_count/hate_count)

# OR
# love_count <- sum(grepl("love", twitter))
# hate_count <- sum(grepl("hate", twitter))
# love_count / hate_count

#5

lines[grep("\\<biostats\\>", lines)]

#OR
# biostats <- grep("biostats", twitter)
# twitter[biostats]

#6 

grep("\\<A computer once beat me at chess, but it was no match for me at kickboxing\\>", lines)

#OR
# sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))


