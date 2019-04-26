library(tidyverse)
library(DT)
library(data.table)
library(yaml)
library(rstudioapi)
library(recommenderlab)
library(Matrix)

source("select_file.R")
source("user_generation.R")
source("posts_generation.R")

# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
user %>% select(starts_with("topic")) -> mat_user
news_posts %>% select(starts_with("topic")) -> mat_posts
cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = config$n_newsposts)
for(i in 1:config$n_users) {
  for(j in 1:config$n_newsposts) {
    cosine_matrix[i,j] <- lsa::cosine(unlist(mat_user[i,]), unlist(mat_posts[j,]))
  }
}




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



m <- matrix( sample(c(0), config$n_newsposts*config$n_users, replace=TRUE),
             ncol=config$n_newsposts,
             dimnames=list(user=paste("u", 1:config$n_users, sep=''), 
                           item=paste("i", 1:config$n_newsposts, sep='')))

ui_matrix <- as(m, "dgCMatrix")
trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
#getRatingMatrix(trainingmatrix)
#image(trainingmatrix, main = "Normalized Ratings")
#hist(getRatings(trainingmatrix), breaks=100)


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
exposure <- matrix(c(0), nrow = config$n_newsposts, ncol = config$n_steps)


# run all simulation steps
pb <- txtProgressBar(min = 0, max = config$n_steps, initial = 0, char = "=",
               width = NA, title="Simulation Run", label, style = 3, file = "")
for (steps in 1:config$n_steps) {
  # for all users
  for(user_id in 1:config$n_users){
    # generate top 10 recommendations 
    
#    user_id <- 3
#    steps <- 1
    
    ui_matrix <- as(m, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    
    rec_sys <- Recommender(trainingmatrix, method=config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
    res <- as(recoms, "list")
    consumed_item <- NA
    if(length(res[[1]])==0){
      consumed_item <- round(runif(1, 1, config$n_newsposts))
    } else {
      consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    # update user interests ----
      
    
    
    # update exposure counts in each step for all recommendations
    
      exposure[consumed_item, steps] + 1 -> temp
      exposure[consumed_item, steps] <- temp
    
  }
  if(steps > 1){
    exposure[,steps] <- exposure[, steps] + exposure[, steps -1]
  }
  image(trainingmatrix, main = "Normalized Ratings")
  setTxtProgressBar(pb, steps)
}
close(pb)




# save results 
results_data <- list(user = user, 
                     news_posts = news_posts, 
                     exposure = exposure)
rds_filename <- config$outputfilename
write_rds(results_data, rds_filename)




