# Post generation

generate_news <- function(config){
  models <- read_rds(here::here("topic_modeling/topicmodels.rds"))
  if(config$n_topics %in% models$k) {
    topic_count <- config$n_topics
  } else {
    topic_count <- max(models$k)
    message(paste0("Number of topics (",config$n_topics,") not availble. Choosing ", topic_count, " topics."))
  }
    
  cov_mat <- models %>% filter(k == topic_count) %>% 
    unique() %>% 
    pull(covmat) %>% 
    unlist() %>% 
    matrix(nrow=topic_count) 
  
  mean_vec <- rep(0.5,nrow(cov_mat))
  
  # create ids 
  total_newsposts <- config$n_newsposts + config$n_newsposts_step * config$n_steps
  news_ids <- 1:total_newsposts
  topic_relevances <- data.frame(news_ids)
  
  # create news posts with certain topics
  #for (i in 1:config$n_topics) {
  # assign topic relevante by uniform distribution
  #  topic_relevance <- data.frame(runif(total_newsposts))
  #names(topic_relevance) <- paste0("topic_", i)
  #topic_relevances <- topic_relevances %>% bind_cols(topic_relevance)
  #}
  relevs <- as_tibble(MASS::mvrnorm(n = total_newsposts, mu = mean_vec, Sigma = cov_mat))
  names(relevs) <- paste0("topic_", 1:config$n_topics)
  topic_relevances <- topic_relevances %>% bind_cols(relevs)
  
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