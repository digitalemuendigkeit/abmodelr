

library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(126)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-1", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(33)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-2", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(133)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-3", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(127)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-4", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(41)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-5", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(154)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-6", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(93)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-7", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(29)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-8", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(1)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-9", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(240)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-10", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(76)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-11", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(80)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-12", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(115)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-13", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(29)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-14", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(30)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-15", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(41)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-16", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(25)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-17", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(89)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-18", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(44)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-19", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(124)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-20", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(22)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-21", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(38)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-22", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(13)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-23", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(80)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-24", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(6)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-25", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(50)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-26", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(109)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-27", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(69)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-28", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(128)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-29", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(5)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-30", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(24)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-31", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(54)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-32", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(43)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-33", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(65)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-34", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(73)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-35", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(115)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-36", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(99)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-37", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(43)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-38", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(124)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-39", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(28)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-40", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(176)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-41", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(56)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-42", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(45)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-43", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(83)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-44", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(117)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-45", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(107)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-46", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(156)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-47", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(116)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-48", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(83)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-49", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(23)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-50", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(27)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-51", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(38)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-52", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(244)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-53", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(80)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-54", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(5)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-55", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(25)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-56", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(62)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-57", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(17)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-58", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(222)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-59", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(126)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-60", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(36)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-61", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(1)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-62", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(94)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-63", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(12)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-64", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(81)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-65", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(24)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-66", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(143)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-67", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(37)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-68", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(25)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-69", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(7)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-70", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(2)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-71", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(26)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-72", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(65)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-73", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(12)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-74", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(66)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-75", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(110)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-76", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(14)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-77", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(12)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-78", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(91)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-79", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(144)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-80", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(80)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-81", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(125)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-82", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(77)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-83", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(22)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-84", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(42)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-85", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(42)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-86", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(100)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-87", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(28)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-88", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(126)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(
  sample(c(0), total_newsposts * config$n_users, replace = TRUE),
  ncol = total_newsposts,
  dimnames = list(
    user = paste("u", 1:config$n_users, sep = ''),
    item = paste("i", 1:total_newsposts, sep = '')
  )
)

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))

# getRatingMatrix(trainingmatrix)
# image(trainingmatrix, main = "Normalized Ratings")
# hist(getRatings(trainingmatrix), breaks = 100)

# create a basic recommendation algorithm
generate_topn_rec <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n) 
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

# generate an empty exposure matrix with rownumbers = posts , colnumbers = simulationsteps
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)
user_record <- list()

# run all simulation steps
pb <- txtProgressBar(
  min = 0, max = config$n_steps, initial = 0, char = "=",
  width = NA, title = "Simulation Run", label, style = 3, file = ""
)

for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts + config$n_newsposts_step  * steps
  current_posts <- m[, 1:n_current_posts]
  
  #generate recommendations once per step
  if (!config$update_for_user) {
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method = config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step)
  
  # for all users
  for (user_id in 1:config$n_users) {
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if (config$update_for_user) {
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id]  # get recommendation from matrix per step
    }
    consumed_item <- NA
    if (user_id %in% first_users) {
      # show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      # print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if (length(res[[1]]) == 0) {
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(
      config$update_user_interest,
      none = {
        # users won't be updated
      },
      random = {
        # update users randomly
        if (rbinom(n = 1, size = 1, prob = config$p_user_update)) {
          user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,2:(config$n_topics+1)]
          # norm users interests

          user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_resource[user_id]
          
        }
      },
      dominant = {
        # update dominant topic
        # find dominant topic
        dominant <- which.max(news_posts[consumed_item, 2:(config$n_topics + 1)])
        # indices shifted by 1 because first column is ID
        user[user_id, dominant + 1] <- user[user_id, dominant+1] + 
          news_posts[consumed_item, dominant + 1]
        # norm users interests
        user[user_id, 2:(config$n_topics + 1)] <- user[user_id, 2:(config$n_topics + 1)] / 
          sum(user[user_id, 2:(config$n_topics + 1)]) * user$interest_resource[user_id]
      }
    )  
    
    # update exposure counts in each step for all recommendations
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  # decrease relevance of old news
  decay_matrix <- diag(
    c(rep(config$decay_factor, n_current_posts), rep(1, total_newsposts - n_current_posts))
  )
  
  m <- t(decay_matrix %*% t(m)) # transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep = '')  # colnames lost after multiplication
  
  if (steps > 1) {
    exposure[, steps] <- exposure[, steps] + exposure[, steps - 1]
    # image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if (config$update_user_interest != "none") {
    # update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  
  psych::describe(user)  %>%  as.data.frame() %>% 
    rownames_to_column() %>%
    filter(str_starts(rowname, "topic")) %>% mutate(step = steps) -> user_record[[steps]]
  setTxtProgressBar(pb, steps)
}
close(pb)

# save results 
results_data <- list(
  user = user, 
  news_posts = news_posts, 
  exposure = exposure,
  user_record = user_record
)
rds_filename <- paste0("1-89", "-", config$outputfilename)
write_rds(path = here::here("runs", "projects", "Default Project", "results", rds_filename), x = results_data)


rm(list = ls())


library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)
library(broom)

source(here::here("runs", "helpers.R"))

config <- load_config("Default Project")[["cond-1.yml"]]


set.seed(65)



# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps

cosine_matrix <- generate_cosine_matrix(user, news_posts)


#' Generate ground truth recommendation for a user_id from a cosine similariy matrix
#'
#' @param user_id the user id (i.e. the row number in the cosine matrix)
#' @param cosine_matrix a cosine similarity matrix where rownumber is the number of users, and colnumer is the number of items
#' @param n how many recommendations to generate
#'
#' @return The ordered recommendations
#'
generate_topn_truth <- function(user_id, cosine_matrix, n = 1) {
  df <- as_tibble(data.frame(t(cosine_matrix)))
  df <- df %>% mutate(id = 1:dim(df)[1]) %>% select(id, user_id)
  suppressMessages(
    res <- top_n(df, n)
  )
  names(res) <- c("id", "match")
  res %>% arrange(desc(match))
}

m <- matrix(Called from: cat(x, file = file, sep = c(rep.int(sep, ncolumns - 1), "\n"), 
    append = append)
