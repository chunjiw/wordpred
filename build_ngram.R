library(tau)
library(magrittr)
library(tm)
library(hash)
library(ggplot2)

# load data
# data availabel here https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
message("load data...")
message(Sys.time())
setwd(getSrcDirectory(function(){}))
news <- readLines("data/en_US/en_US.news.txt", n = -1)
twitter <- readLines("data/en_US/en_US.twitter.txt", n = -1)
blog <- readLines("data/en_US/en_US.blogs.txt", n = -1)
# down sample half 
news <- news[sample(1:length(news), length(news) / 2)]
twitter <- twitter[sample(1:length(twitter), length(twitter) / 10)]
blog <- blog[sample(1:length(blog), length(blog) / 10)]

# preprocessing
message("preprocessing ...")
message(Sys.time())
removeURL <- function(x) {gsub("http\\S+", "", x)}
removeHash <- function(x) {gsub("[@#&]\\S+", "", x)}
# don't know how to remove location, like "@ los angeles"
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]']*", "", x)
twitter <- twitter %>% removeURL() %>% removeHash() %>% 
  removeNumPunct() %>% tolower() %>% stripWhitespace()
news <- news %>% removeURL() %>% removeHash() %>% 
  removeNumPunct() %>% tolower() %>% stripWhitespace()
blog <- blog %>% removeURL() %>% removeHash() %>% 
  removeNumPunct() %>% tolower() %>% stripWhitespace()
all <- c(twitter, blog, news)
remove(twitter, blog, news)

# get ngram
message("get trigram ...")
message(Sys.time())
trigram <- textcnt(all, n=3, split=" ", method="string", decreasing = TRUE) 

# create hash table for fast prediction
message("create hash ...")
message(Sys.time())
h = hash()
for (i in 1:length(trigram)) {
  if (trigram[i] < 4) {
    break
  } else {
    name <- strsplit(names(trigram[i]), split = ' ')[[1]]
    key <- paste0(name[1:2], collapse = ' ')
    if (!has.key(key, h)) {
      h[[key]] <- name[3]
    } else if (length(h[[key]]) < 15) {
      h[[key]] <- append(h[[key]], name[3])
    }
  }
}
# remove(trigram)

# save ngram model and hash
save(h, file = "hashtable.Rdata")
save(trigram, file = "trigram.Rdata")
