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
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]']*", "", x)
h <- hash()

# loop over batches
# for (b in 1:nbatch - 1) {
for (b in 12) {
  print(sprintf("Processing the %i-th batch", b))
  
  # concatenate text and preprocess
  blog <- blog.full[blog.len * b + (1:blog.len)]
  news <- news.full[news.len * b + (1:news.len)]
  twit <- twit.full[twit.len * b + (1:twit.len)]
  bnt <- c(twit, blog, news) %>% 
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
  save(trigram, file = "trigram.Rdata")
}