## A module to generate topic models (their covariant appearance) from real world data.
# Requires the All the news dataset

library(tidyverse)
library(tidytext)
library(ldatuning)
library(topicmodels)
library(purrr)

### WARNING

## Runtime of CTM is O(k^3). Do not use to large topic models. The document size

# How man randomized local starts to use
rand_local_starts <- 1
# How many documents to include
doc_size <- 200
# How many topics to model
para <- tibble(k = c(3,5,7))

data <- read_csv("topic_modeling/all-the-news/articles1.csv", col_types = "ddccccddcc")
data <- read_csv("topic_modeling/all-the-news/articles2.csv", col_types = "ddccccddcc") %>% bind_rows(data)
data <- read_csv("topic_modeling/all-the-news/articles3.csv", col_types = "ddccccddcc") %>% bind_rows(data)


#
# News Outlets
data %>% ggplot() + aes(x=publication) + geom_bar() + coord_flip() + labs(title="Amount of Newsposts per publisher")


# Paralelization prep
max(parallel::detectCores() -1, 10) -> cores_count


# Load a list of stopwords
mystopwords <- tibble(word=stopwords::stopwords("en", source = "stopwords-iso")) 



# select only "Fox News" for topic modeling (Small data set, relatively diverse topics)
text_df <- data %>% filter(publication == "Fox News") %>% select(title, publication, content, year) %>% 
  sample_n(doc_size)



# Tokenization on the word level
tokens <- text_df %>% unnest_tokens(word, content)

# Get a set of unique words that are cleaned up
lemma_unique <- tokens  %>%
  select(word) %>%
  # remove 's genetive
  mutate(word_clean = str_replace_all(word,"\u2019s|'s","")) %>% 
  # remove numerics
  mutate(word_clean = ifelse(str_detect(word_clean,"[^[:alpha:]]"),NA,word_clean)) %>%
  filter(!duplicated(word_clean)) %>%
  filter(!is.na(word_clean)) %>%
  arrange(word)

# Lammatization of unique set
lemma_unique<-lemma_unique %>%
  mutate(word_stem = textstem::lemmatize_words(word_clean))

# lemmatization of corpus
lemmatized_counts <- tokens %>% 
  mutate(stem = textstem::lemmatize_words(word)) %>% 
  left_join (lemma_unique %>% 
  select(word, word_stem)) %>% 
  filter(!is.na(word_stem)) %>% 
  count(title, stem, sort = TRUE) %>% 
  ungroup()

# remove stopwords from lemmatized corpus
lemmatized_cln <- lemmatized_counts %>% 
  rename(word = stem) %>% 
  anti_join(mystopwords) %>% 
  add_count(word, name = "all")

# Bigrams maybe?
#lemmatized_counts_ngram <- text_df %>% 
#  unnest_tokens(bigram, content, token = "ngrams", n = 3, n_min = 2) %>% 
#  count(title, bigram, sort = TRUE) %>% 
#  ungroup()

dtm <- lemmatized_cln %>% cast_dtm(title, word, n)

### Topic modelling
control_list_ctm <- list(
  # random start
  seed = NA, 
  # multistart local search from 1 location (faster, potential local optimum)
  nstart = rand_local_starts,
  # output all 2 CG iterations
  verbose = 2,
  # Stop after 500 iterations
  cg = list(iter.max = 500, tol = 10^-5),
  initialize = "random",
  best = TRUE
)



# Conduct the CTM ----
system.time(
models <- para %>% mutate(ctm = map(k, function(k) CTM(
  dtm, k, control = control_list_ctm
)))
)




# Clean the models ----


# Tidy functions to get gamma and beta 
tidy_ctm_gamma  <- function(CTM_object){
  CTM_object %>% 
    slot("gamma")  %>% 
    as_tibble()  %>% 
    mutate (document = row_number()) %>% 
    gather(topic, gamma, -document) %>%
    mutate(topic = strtoi(stringr::str_sub(topic,2)))
}

tidy_ctm_beta  <- function(CTM_object){
  Terms  <- CTM_object %>% 
    slot("terms") 
  
  CTM_object %>% 
    slot("beta")  %>% 
    as_data_frame() %>%
    setNames(Terms) %>%
    mutate (topic = row_number()) %>% 
    gather(term, beta, -topic) %>%
    mutate(beta = exp(beta))
}

# Statistics about the model
models <- models %>% mutate(perp = perplexity(ctm))

# Calculate Gamma and Beta
models <- models %>%
  mutate(ctm_beta = map(.x=ctm, .f=tidy_ctm_beta)) %>% 
  mutate(ctm_gamma = map(.x=ctm, .f=tidy_ctm_gamma)) #%>% 
  #mutate(covmat = map(.x =ctm_gamma, .f=cov))


# function to create covariance matrix from gamma_matrix ----
tidy_cov_mat_from_gamma <- function(gamma_matrix) {
  gamma_matrix %>% 
    spread(topic, gamma) %>% 
    select(-document) %>% 
    cov()
}

# Map covariance matrix
models <- models %>% mutate(covmat = map(.x= ctm_gamma, .f=tidy_cov_mat_from_gamma))


write_rds(models, "topic_modeling/topicmodels.rds")

# Plotting gamma distributions 
plot_gamma_dist <- function(gamma_matrix, name) {
  p <- gamma_matrix %>% 
    ggplot() +
    aes(x=gamma) +
    geom_histogram() + facet_wrap(~topic) 
  p
}
models %>% pull(ctm_gamma) %>% map(.f = plot_gamma_dist)


## Select a model to visualize the top terms
models %>% pull(ctm_beta) %>% `[[`(3)  %>% group_by(topic) %>% top_n(10, beta) %>% 
  arrange(topic, -beta) %>% mutate( rank = row_number()) %>% 
  ungroup() %>%
  select(-beta) %>% 
  spread(rank, term)

### sav the covariance matrix for topic model generation in ABM
topic_count <- 3
models %>% filter(k == topic_count) %>% 
  unique() %>% 
  pull(covmat) %>% 
  unlist() %>% 
  matrix(nrow=topic_count) %>% write_rds("covmat.rds")




### look at http://www.bernhardlearns.com/2017/05/topic-models-lda-and-ctm-in-r-with.html for ctm explanation
library(topicmodels)
