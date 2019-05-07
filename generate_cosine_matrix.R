#generate cosine matrix to compare user interest to news posts

generate_cosine_matrix <- function(user, news_posts){
  user %>% select(starts_with("topic")) -> mat_user
  news_posts %>% select(starts_with("topic")) -> mat_posts
  cosine_matrix <- matrix(c(0), nrow = config$n_users, ncol = total_newsposts)
  for(i in 1:config$n_users) {
    for(j in 1:total_newsposts) {
      cosine_matrix[i,j] <- lsa::cosine(unlist(mat_user[i,]), unlist(mat_posts[j,]))
    }
  }
  cosine_matrix
}
