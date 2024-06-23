#' fetches comments made by these authors across different subreddits within a specified time frame
#' calculates the total score of comments per subreddit for each author
#' identifies the top subreddits based on these scores
#' aggregates their activity across other subreddits, and saves this data to a CSV file

#load necessary libraries
library(tidyverse)

#get data from a given endpoint and handle potential errors
get_data <- function(endpoint) {
  json_data <- tryCatch(jsonlite::fromJSON(endpoint),
      error={
        print("error") #print error message and retry after 1 second if an error occurs
        Sys.sleep(1)
        jsonlite::fromJSON(endpoint) #retry fetching the data
      })
  
  tbl_data <- json_data %>%
    .$data %>%
    jsonlite::flatten(recursive = TRUE) %>%
    select(author, body, parent_id, score, created_utc, subreddit) %>%
    as_tibble() #convert the data into a tibble for easier manipulation
  
  return(tbl_data)
}

#get up to 100 comments by a specific author after a given time
get_comments_100 <- function(author, after_time) {
  endpoint <- "https://api.pushshift.io/reddit/search/comment/?size=100&score=%3E9&author="
  endpoint <- paste0(endpoint, author, "&after=", after_time) #construct the endpoint URL
  data <- get_data(endpoint) #fetch the data
  return(data)
}

#get comments by an author within a specific time frame
get_comments <- function(author, after_time, before_time) {
  data <- get_comments_100(author, after_time) #initial fetch
  while(data[nrow(data),"created_utc"] < before_time) { #keep fetching until reaching the before_time
    nrows <- nrow(data)
    try(data <- bind_rows(data, get_comments_100(author, data[nrows,"created_utc"]))) #attempt to append new data
    nrows_new <- nrow(data)
    if (nrows == nrows_new) { #break loop if no new data is added
      return(data)
    }
  }
  return(data)
}

#get subreddit activity for a given author within a specified time frame
get_subs <- function(author) {
  posts <- get_comments(author, 1546347661, 1641042061) #fetch comments by the author
  subreddits <- posts %>%
    group_by(subreddit) %>%
    summarise(score_sum = sum(score)) %>%
    arrange(desc(score_sum)) %>%
    mutate(author=author) #aggregate scores by subreddit and add author column
  return(subreddits)
}

#read existing comments data
comments <- read_csv("antiwork_comments.csv")

#aggregate authors by total score
authors <- comments %>%
  group_by(author) %>%
  summarise(sum_score = sum(score)) %>%
  arrange(desc(sum_score))

#calculate top 10% of authors
top10num <- as.integer(nrow(authors) / 10)
top10pct <- authors$author[3:201] #select authors in the top 10%

#initialize data collection for the top 10% authors
sub_data <- get_subs(authors$author[2])
for (i in seq(1:200)){
  sub_data <- bind_rows(sub_data, get_subs(top10pct[i])) #aggregate subreddit data for each author
  print(i) #print current iteration for tracking progress
}

#arrange the final data by descending score_sum
sub_data <- arrange(sub_data, desc(score_sum))

#write the aggregated subreddit data to a csv file
write_csv(sub_data, "assets/subreddit_data.csv")