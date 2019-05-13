# User setup 


generate_users <- function(config){
  user_ids <- 1:config$n_users
  # initialize interest_resource with 3 time the amount of topics. So users have haed_room to increase their interest
  interest_resource <- rep(config$n_topics * 3, config$n_users)
  # setup a data.frame for all users
  topic_interests <- data.frame(user_ids)
  
  # Generate interests for the users by topic
  for (i in 1:config$n_topics) {
    # randomly assign interest to topics using the uniform distribution
    topic_interest_level <- data.frame(runif(config$n_users))
    # assign sensible column names
    names(topic_interest_level) <- paste0("topic_", i)
    # bind all topic interest to the user
    topic_interests <- topic_interests %>% bind_cols(topic_interest_level)
  }
  
  # create the actual user data frame
  user <- data.frame(topic_interests, interest_resource)
  user
}
