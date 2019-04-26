# Post generation

generate_news <- function(config){
  # create ids 
  total_newsposts <-config$n_newsposts + config$n_nesposts_step * config$n_steps
  news_ids <- 1:total_newsposts
  topic_relevances <- data.frame(news_ids)
  
  # create news posts with certain topics
  for (i in 1:config$n_topics) {
    # assign topic relevante by uniform distribution
    topic_relevance <- data.frame(runif(total_newsposts))
    names(topic_relevance) <- paste0("topic_", i)
    topic_relevances <- topic_relevances %>% bind_cols(topic_relevance)
  }
  
  news_posts <- data.frame(topic_relevances)
  # calculate the sum of topic interests, to measure the "likeability"(?) of post
  news_posts %>% select(starts_with("topic")) %>% mutate(sumtopics = rowSums(.)) %>% select(sumtopics) -> sumcol
  news_posts %>% bind_cols(sumcol) -> news_posts
  
  
  # Define a random likebility score for all news posts (uniform distribution, between 0 and topic-limit)
  news_scores <- runif(total_newsposts, min = 1, max = config$topic_limit)
  # get all topic values into a matrix for normalization
  news_posts %>% select(starts_with("topic")) -> matrix_of_initial_values
  # normalize all topic-value by the rowwise sum (generated above) - now sums should be 1
  updated_topics <- (matrix_of_initial_values / t(sumcol)) 
  
  # update the news_posts
  news_posts <- bind_cols(data.frame(news_ids), updated_topics, data.frame(news_scores))
  news_posts
}