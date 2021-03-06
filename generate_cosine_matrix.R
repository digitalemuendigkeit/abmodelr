# generate cosine matrix to compare user interest to news posts
veclength <- function(x) {
  sqrt(x %*% x)
}

generate_cosine_matrix <- function(user, news_posts) {
  #' generate a cosine matrix
  #' 
  #' @param user a user data frame
  #' @param news_posts a news post data frame
  #' 
  #' @return cosine_matrix: the generated cosine matrix
  
  # select topic values of user
  user %>% select(starts_with("topic")) %>% as.matrix() -> mat_user
  
  # select topic values of news posts
  news_posts %>% select(starts_with("topic"))%>% as.matrix() -> mat_posts
  
  # initialize cosine matrix
  cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = total_newsposts)
  
  # rowwise normalization to simplify cosine
  mat_user_norm <- diag(1/apply(mat_user, 1, veclength)) %*% mat_user 
  mat_post_norm <- diag(1/apply(mat_posts, 1, veclength)) %*% mat_posts 

  # now only matrix mult  
  cosine_matrix <- mat_user_norm %*% t(mat_post_norm)
  
  cosine_matrix
  
}
