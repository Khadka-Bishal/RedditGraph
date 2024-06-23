#' collect comments from the Reddit subreddit antiwork using the Pushshift API
#' fetches comments that have a score greater than 10, within specified time intervals

#load the necessary library
library(tidyverse)

#function to get comments between two timestamps
get_comments_between_times <- function(start_time, end_time) {
  #initial fetch of up to 100 comments after the start_time
  comments_data <- fetch_100_comments(start_time)
  #keep fetching and appending comments until the last comment's timestamp is before the end_time
  while(comments_data[nrow(comments_data), "created_utc"] < end_time) {
    last_comment_time <- comments_data[nrow(comments_data), "created_utc"]
    new_comments <- fetch_100_comments(last_comment_time)
    comments_data <- bind_rows(comments_data, new_comments)
    print(as.numeric(last_comment_time)) #print the timestamp of the last comment fetched for debugging
  }
  return(comments_data)
}

fetch_100_comments <- function(after_time) {
  #construct the api endpoint url
  api_endpoint <- paste0("https://api.pushshift.io/reddit/comment/search/?subreddit=antiwork&size=100&score=%3E10&after=", after_time)
  #fetch data from the api, parse json, and select relevant columns
  api_endpoint %>%
    jsonlite::fromJSON() %>%
    .$data %>%
    jsonlite::flatten(recursive = TRUE) %>%
    select(author, body, parent_id, score, created_utc, subreddit) %>%
    as_tibble() #convert to tibble for easier manipulation
}

#read existing comments data from a csv file
existing_comments <- read_csv("antiwork_comments.csv")

#fetch comments for two specific time intervals
comments_batch_1 <- get_comments_between_times(1639042061, 1641042061)
comments_batch_2 <- get_comments_between_times(1631042061, 1641042061)

#combine the new comments with the existing ones
all_comments <- bind_rows(existing_comments, comments_batch_1)

#write the combined comments back to the csv file
write_csv(all_comments, "assets/antiwork_comments.csv")