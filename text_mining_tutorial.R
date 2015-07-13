rm(list=ls())
library(tm) # Framework for text mining.
library(Matrix)
library(dplyr) # Data preparation and pipes %>%.

# Load a corpus
raw_data <- read.csv("./raw_train_test.csv",stringsAsFactors = FALSE)

# corpus <- Corpus(
#   DataframeSource(
#     (raw_data %>% select(query,product_title))))

queries <- Corpus(DataframeSource((raw_data %>% select(query))))
titles <- Corpus(DataframeSource((raw_data %>% select(product_title))))
corpus <- Corpus(DataframeSource((raw_data %>% select(query,product_title))))

transform_corpus <- function(corpus){
  # Transformations:
  corpus <- corpus %>% 
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(removeNumbers)) %>%
    tm_map(removeWords,stopwords('english')) %>%
    tm_map(content_transformer(stripWhitespace)) %>%
    tm_map(content_transformer(removePunctuation)) %>%
    tm_map(stemDocument)
  return(corpus)   
}

queries <- transform_corpus(queries)
titles <- transform_corpus(titles)
corpus <- transform_corpus(corpus)

query_dtm <- DocumentTermMatrix(
  queries,
  control = list(
    weighting = function(x){weightTfIdf(x, normalize = FALSE)},
    stopwords = TRUE)
)
title_dtm <- DocumentTermMatrix(
  titles,
  control = list(
    weighting = function(x){weightTfIdf(x, normalize = FALSE)},
    stopwords = TRUE)
)
dtm <- DocumentTermMatrix(
  corpus,
  control = list(
    weighting = function(x){weightTfIdf(x, normalize = FALSE)},
    stopwords = TRUE)
)

query_dtm
title_dtm
corpus

