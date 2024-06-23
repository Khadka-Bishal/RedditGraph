#' preprocesses the text data by cleaning and filtering
#' applies Structural Topic Modeling (STM) to identify prevalent topics within the comments
#' visualizes the top terms associated with each identified topic to provide insights into the main themes discussed in the comments

#libraries for text processing and analysis
library(tidytext)
library(tidyverse)
library(dplyr)
library(sm)
library(stm)
library(lubridate)

#function to load and preprocess comments data
load_and_preprocess_comments <- function() {
    comments <- read.csv("antiwork_comments.csv") 
    words_to_remove <- read.csv("stopwords.csv")
    words_to_remove <- unlist(as.list(words_to_remove$word))
    comments$body <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035]","",comments$body)
    comments$body <- gsub("\'","", comments$body)
    comments <- data.frame(comments$body)
    antiwork <- read.csv("antiwork_comments.csv")
    antiwork <- cbind(comments, antiwork) 
    antiwork <- antiwork %>% select(c(2,1,4,5,6,7))
    return(antiwork)
}

#function to convert utc to ymd and filter by comment length
convert_and_filter <- function(antiwork) {
    antiwork$Date <- as.Date(as_datetime(antiwork$created_utc, tz = "UTC"))
    antiwork$date_numeric <- as.numeric(antiwork$Date)
    antiwork <- antiwork %>% filter(str_length(comments) > 200)
    return(antiwork)
}

#function to process text for topic modeling
process_text_for_modeling <- function(antiwork, words_to_remove) {
    antiwork.processed <- textProcessor(antiwork$comments, metadata = antiwork, customstopwords=words_to_remove, striphtml = TRUE, removenumbers = TRUE, language = "en")
    out <- prepDocuments(antiwork.processed$documents, antiwork.processed$vocab, antiwork.processed$meta, lower.thresh = 100)
    return(out)
}

#function to perform topic modeling and plot results
perform_topic_modeling_and_plot <- function(out) {
    antiwork.topic.modeling <- stm(documents = out$documents, 
                                   vocab = out$vocab,
                                   K = 15, 
                                   max.em.its = 75, 
                                   prevalence =~ score + s(date_numeric),
                                   data = out$meta,
                                   init.type = "Spectral", 
                                   verbose = FALSE)
    plot(antiwork.topic.modeling, n=11)
}

#main execution flow
antiwork <- load_and_preprocess_comments()
antiwork <- convert_and_filter(antiwork)
out <- process_text_for_modeling(antiwork, words_to_remove)
perform_topic_modeling_and_plot(out)

