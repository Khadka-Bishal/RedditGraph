#' calculates the frequency of each word and arranges them in descending order

#load necessary libraries
library(tidyverse)
library(lubridate)
library(tidytext)

#load reddit comments data and convert utc to datetime
reddit_df <- read_csv('./assets/antiwork.csv')
reddit_df$date_col <- as_datetime(reddit_df$created_utc, tz = "UTC")

#tokenize comments, remove stop words, and clean text
anti_comments <- reddit_df %>%
    select(author, body, created_utc) %>%
    unnest_tokens(word, body) %>% #tokenize text
    anti_join(stop_words) %>% #remove stop words
    filter(!str_detect(word, "^\\d+$")) %>% #remove numbers
    mutate(word = str_replace_all(word, "[_\\s'\u2018\u2019\u201A\u201B\u2032\u2035]", "")) #clean special characters and whitespace

#count and arrange word frequency
anti_comments <- anti_comments %>%
    group_by(word) %>%
    count() %>%
    arrange(desc(n))

#create sequences of start and end dates for years 2019 to 2021
start_years <- seq(as.Date("2019/1/1"), as.Date("2021/1/1"), "years")
end_years <- seq(as.Date("2019/12/31"), as.Date("2021/12/31"), "years")