library(tidyverse)
library(tau)
library(tm)
library(hash)

# load data
# data availabel here https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
message("load data...")
message(Sys.time())
setwd(getSrcDirectory(function(){}))
blog.full <- readLines("data/en_US/en_US.blogs.txt")
news.full <- readLines("data/en_US/en_US.news.txt")
twit.full <- readLines("data/en_US/en_US.twitter.txt")
# batch size
nbatch <- 50
blog.len <- ceiling(length(blog.full) / nbatch)
news.len <- ceiling(length(news.full) / nbatch)
twit.len <- ceiling(length(twit.full) / nbatch)

# preprocessing functions
removeURL <- function(x) gsub("http\\S+", "", x)
removeHash <- function(x) gsub("[@#&]\\S+", "", x)
# don't know how to remove location, like "@ los angeles"
removeNumPunct <- function(x) gsub("[^A-z[:space:]']*", "", x)
# it turns out that Japanese characters belong to [:alpha:], but not [A-z]. Absurd.
h <- hash()

# loop over batches
for (b in 1:nbatch - 1) {
# for (b in 12932) {
  message(sprintf("Processing the %i-th batch", b))
  
  # concatenate text and preprocess
  blog <- blog.full[blog.len * b + (1:blog.len)]
  news <- news.full[news.len * b + (1:news.len)]
  twit <- twit.full[twit.len * b + (1:twit.len)]
  bnt <- c(blog, news, twit) %>% 
    removeURL() %>% removeHash() %>% 
    removeNumPunct() %>% tolower() %>% stripWhitespace()
  
  # obtain trigram
  trigram <- textcnt(bnt, n=3, split=" ", method="string", decreasing = TRUE) 
  
  # create hash table for fast prediction
  for (i in 1:length(trigram)) {
    if (trigram[i] < 4) {
      break
    } else {
      gram <- strsplit(names(trigram[i]), split = ' ')[[1]]
      history <- paste0(gram[1:2], collapse = ' ')
      candidate <- gram[3]
      count <- trigram[[i]]
      if (candidate %in% h[[history]]$candidate) {
        index <- h[[history]]$candidate == candidate
        h[[history]]$count[index] <- h[[history]]$count[index] + count
      } else {
        h[[history]]$candidate <- c(h[[history]]$candidate, candidate)
        h[[history]]$count <- c(h[[history]]$count, count)
      }
    }
  }
  gc()
}

if (0) {
  save(h, file = "hashtable.Rdata")
  save(h, file = "wordpred/hashtable.Rdata")
  save(trigram, file = "trigram.Rdata")
}