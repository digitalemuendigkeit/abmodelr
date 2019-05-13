#generate cosine matrix to compare user interest to news posts
veclength <- function(x){
  sqrt(x %*% x)
}


generate_cosine_matrix <- function(user, news_posts){
  
  user %>% select(starts_with("topic")) %>% as.matrix() -> mat_user
  news_posts %>% select(starts_with("topic"))%>% as.matrix() -> mat_posts
  cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = total_newsposts)
  
  # rowwise normalization to simplify cosine
  mat_user_norm <-  diag(1/apply(mat_user, 1, veclength)) %*% mat_user 
  mat_post_norm <- diag(1/apply(mat_posts, 1, veclength)) %*% mat_posts 

  # now only matrix mult  
  cosine_matrix <- mat_user_norm %*% t(mat_post_norm)
  
  cosine_matrix
}
