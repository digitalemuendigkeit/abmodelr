library(tidyverse)
library(data.table)
library(yaml)
library(rstudioapi)

source("select_file.R")



# read results 
rds_filename <- config$outputfilename
results_data <- read_rds(rds_filename)
user <- results_data$user
exposure <- results_data$exposure
news_posts <- results_data$news_posts


# analyze results ----
user %>% ggplot() +
  aes(topic_1) + geom_histogram()


names(exposure) <- paste0("V", 1:dim(exposure)[2])
df <- as_tibble(exposure) %>% 
  rownames_to_column() %>% 
  rename(news_post = rowname) %>% 
  gather(step, value, -news_post) %>% 
  mutate(step = str_remove(step, "V")) %>% 
  mutate(step = as.numeric(step)) %>% 
  mutate(news_post = factor(news_post))
  

df %>% ggplot() +
  aes(x = step, y = value, color = news_post) +
  geom_line(alpha = 0.1) +
  guides(color = FALSE) 

