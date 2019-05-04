## A module to generate topic models (their covariant appearance) from real world data.
# Requires the All the news dataset

library(tidyverse)
library(tidytext)
library(ldatuning)

data <- read_csv("topic_modeling/all-the-news/articles1.csv", col_types = "ddccccddcc")
data <- read_csv("topic_modeling/all-the-news/articles2.csv", col_types = "ddccccddcc") %>% bind_rows(data)
data <- read_csv("topic_modeling/all-the-news/articles3.csv", col_types = "ddccccddcc") %>% bind_rows(data)



# News Outlets
data %>% ggplot() + aes(x=publication) + geom_bar() + coord_flip()


# Paralelization prep
max(parallel::detectCores() -1, 10) -> cores_count
mystopwords <- tibble(word=stopwords::stopwords("en", source = "stopwords-iso")) 

data %>% filter(publication == "Fox News") %>% select(title, publication, content, year) -> text_df

text_df %>% 
  unnest_tokens(word, content) %>% 
  mutate(stem = textstem::lemmatize_words(word)) %>% 
  count(title, stem, sort = TRUE) %>% 
  ungroup() -> lemmatized_counts

lemmatized_cln <- lemmatized_counts %>% rename(word = stem) %>% anti_join(mystopwords) 

lemmatized_counts_ngram <- text_df %>% 
  unnest_tokens(bigram, content, token = "ngrams", n = 3, n_min = 2) %>% 
  count(title, bigram, sort = TRUE) %>% 
  ungroup()

dtm <- lemmatized_cln %>% cast_dtm(title, word, n)

dtm

control_list_gibbs <- list(
  burnin = 2500,
  iter = 5000,
  seed = 0:4,
  nstart = 5,
  best = TRUE,
  verbose = 100
)

topic_number_lemma <- FindTopicsNumber(
  dtm,
  topics = c(seq(from = 2, to = 9, by = 1)),
  metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = control_list_gibbs,
  mc.cores = cores_count,
  verbose = TRUE
)

FindTopicsNumber_plot(topic_number_lemma)

LDA(
  k=k, 
  x=dtm, 
  method="Gibbs", 
  control=control_list_gibbs
)