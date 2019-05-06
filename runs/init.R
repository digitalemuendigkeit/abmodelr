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
source("generate_cosine_matrix.R")

set.seed(0)

# User generation ----
user <- generate_users(config)

# posts generation  ----
news_posts <- generate_news(config)


# initilize ground truth
total_newsposts <-config$n_newsposts + config$n_newsposts_step  * config$n_steps

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



m <- matrix( sample(c(0), total_newsposts*config$n_users, replace=TRUE),
             ncol=total_newsposts,
             dimnames=list(user=paste("u", 1:config$n_users, sep=''), 
                           item=paste("i", 1:total_newsposts, sep='')))

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
exposure <- matrix(c(0), nrow = total_newsposts, ncol = config$n_steps)


# run all simulation steps
pb <- txtProgressBar(min = 0, max = config$n_steps, initial = 0, char = "=",
                     width = NA, title="Simulation Run", label, style = 3, file = "")
for (steps in 1:config$n_steps) {
  
  #select currently relevant posts
  n_current_posts <- config$n_newsposts+config$n_newsposts_step  * steps
  current_posts <- m[ ,1:n_current_posts]
  
  #generate recommendations once per step
  if(!config$update_for_user){
    ui_matrix <- as(current_posts, "dgCMatrix")
    trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
    rec_sys <- Recommender(trainingmatrix, method=config$recommender)
    recoms <- recommenderlab::predict(rec_sys, trainingmatrix, n = 10)
  }
  
  #draw users that are randomly shown new content
  first_users <- sample(1:config$n_users, config$n_newsposts_step )
  
  # for all users
  for(user_id in 1:config$n_users){
    # generate top 10 recommendations 
    
    #update recommendation after each user
    if(config$update_for_user){
      ui_matrix <- as(current_posts, "dgCMatrix")
      trainingmatrix <- (new("realRatingMatrix", data = ui_matrix))
      
      rec_sys <- Recommender(trainingmatrix, method=config$recommender)
      recoms <- recommenderlab::predict(rec_sys, trainingmatrix[user_id], n = 10)
      res <- as(recoms, "list")
    } else {
      res <- as(recoms, "list")[user_id] #get recommendation from matrix per step
    }
    consumed_item <- NA
    if(user_id %in% first_users){
      #show user new item
      consumed_item <- n_current_posts - config$n_newsposts_step + match(user_id, first_users)
      #print(paste("Show item ", consumed_item , " to user ", user_id))
    } else {
      if(length(res[[1]])==0){
        consumed_item <- round(runif(1, 1, n_current_posts))
      } else {
        consumed_item <- as.numeric(str_remove(res[[1]][[1]], "i"))
      }
    }
    
    evaluation <- (cosine_matrix[user_id, consumed_item] * 5)
    m[user_id, consumed_item] <- evaluation
    
    
    # update user interests ----
    switch(config$update_user_interest,
           none={
             #users won't be updated
           },
           random={
             #update users randomly
             if(rbinom(n=1, size=1, prob=config$p_user_update)){
               user[user_id,2:(config$n_topics+1)] <-user[user_id,2:(config$n_topics+1)]+news_posts[consumed_item,(config$n_topics+1)]
               #norm users interests
               user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_ressource[user_id]
             }
           },
           dominant={
             #update dominant topic
             #find dominant topic
             dominant <- which.max(news_posts[consumed_item,2:(config$n_topics+1)])
             #indices shifted by 1 because first column is ID
             user[user_id, dominant+1] <- user[user_id, dominant+1] + news_posts[consumed_item, dominant+1]
             #norm users interests
             user[user_id,2:(config$n_topics+1)] <- user[user_id,2:(config$n_topics+1)] / sum(user[user_id,2:(config$n_topics+1)]) *user$interest_ressource[user_id]
           }
    )  
    
    
    # update exposure counts in each step for all recommendations
    
    exposure[consumed_item, steps] + 1 -> temp
    exposure[consumed_item, steps] <- temp
    
  }
  
  #decrease relevance of old news
  decay_matrix <- diag(c(rep(config$decay_factor, n_current_posts),rep(1, total_newsposts - n_current_posts)))
  
  m <-t(decay_matrix %*% t(m)) #transposed as dimensions won't fit otherwise
  colnames(m) <- paste("i", 1:total_newsposts, sep='') #colnames lost after multiplication
  
  if(steps > 1){
    exposure[,steps] <- exposure[, steps] + exposure[, steps -1]
    #image(trainingmatrix, main = "Normalized Ratings")
  }
  
  if(config$update_user_interest != "none"){
    #update cosine matrix
    cosine_matrix <- generate_cosine_matrix(user, news_posts)
  }
  setTxtProgressBar(pb, steps)
}
close(pb)




# save results 
results_data <- list(user = user, 
                     news_posts = news_posts, 
                     exposure = exposure)
rds_filename <- config$outputfilename
write_rds(results_data, rds_filename)
