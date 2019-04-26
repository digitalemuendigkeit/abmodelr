# run all simulation steps
pb <- txtProgressBar(min = 0, max = config$n_steps, initial = 0, char = "=",
                     width = NA, title="Simulation Run", label, style = 3, file = "")
for (steps in 1:config$n_steps) {
  # for all users
  for(user_id in 1:config$n_users){
    # generate top 10 recommendations 
    recs <- generate_topn_rec(user_id, cosine_matrix = cosine_matrix, 10)
    
    
    
    # update user interests ----
    # draw one random sample from recommendations to consume
    i <- sample(1:dim(recs)[1], 1)
    # get position of newspost in news post data frame
    position <- unlist(recs[i,1] )
    
    # get scores from posts
    news_posts[position,] %>% select(starts_with("topic")) -> post_scores
    # get user interests
    user[user_id, ] %>% select(starts_with("topic")) -> user_interest
    # update user_interest from posts
    user_interest <- user_interest + post_scores
    
    #noramlize if interest_ressource is maxed out
    if(sum(user_interest) > user[user_id,]$interest_ressource){
      user_interest <- user_interest / sum(user_interest) * user[user_id,]$interest_ressource
    }
    
    # Update only top topics?
    
    # update user interest
    user[user_id,]  <- c(user_ids = user_id, user_interest, user[user_id,]$interest_ressource)     
    
    
    
    # update exposure counts in each step for all recommendations
    position <- unlist(recs[i,1] )
    exposure[position, steps] + 1 -> temp
    exposure[position, steps] <- temp
    
  }
  if(steps > 1){
    exposure[,steps] <- exposure[, steps] + exposure[, steps -1]
  }
  setTxtProgressBar(pb, steps)
}
close(pb)