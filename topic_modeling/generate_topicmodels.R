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

tokens <- text_df %>% unnest_tokens(word, content)

# Get a set of unique words that are cleaned up
lemma_unique <- tokens  %>%
  select(word) %>%
  mutate(word_clean = str_replace_all(word,"\u2019s|'s","")) %>%
  mutate(word_clean = ifelse(str_detect(word_clean,"[^[:alpha:]]"),NA,word_clean)) %>%
  filter(!duplicated(word_clean)) %>%
  filter(!is.na(word_clean)) %>%
  arrange(word)

lemma_unique<-lemma_unique %>%
  mutate(word_stem = textstem::lemmatize_words(word_clean))

tokens %>% 
  mutate(stem = textstem::lemmatize_words(word)) %>% 
  left_join (lemma_unique %>% 
               select(word, word_stem)) %>% filter(!is.na(word_stem)) %>% 
  count(title, stem, sort = TRUE) %>% 
  ungroup() -> lemmatized_counts

lemmatized_cln <- lemmatized_counts %>% rename(word = stem) %>% 
  anti_join(mystopwords) %>% 
    add_count(word, name = "all")

#lemmatized_counts_ngram <- text_df %>% 
#  unnest_tokens(bigram, content, token = "ngrams", n = 3, n_min = 2) %>% 
#  count(title, bigram, sort = TRUE) %>% 
#  ungroup()

### ALL WORDS
dtm <- lemmatized_cln %>% cast_dtm(title, word, n)
#View(lemmatized_cln)
#dtm <- lemmatized_cln %>% filter(all > 700) %>% cast_dtm(title, word, n)

dtm



### Topic modelling
control_list_ctm <- list(
  seed = NA,
  nstart = 1,
  verbose = 2,
  cg = list(iter.max = 500, tol = 10^-5),
  initialize = "random",
  best = TRUE
)
library(topicmodels)
library(purrr)

para <- tibble(k = c(3,5,7,9))

models <- para %>% mutate(ctm = map(k, function(k) CTM(
  dtm, k, control = control_list_ctm
)))


model <- topicmodels::CTM(dtm, 5, control = control_list_ctm)

model <- topicmodels::CTM(dtm, 30)
perplexity(model)

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


tidy_ctm_gamma(model) %>% ggplot(aes(x=gamma)) + geom_histogram() + facet_wrap(~topic)
tidy_ctm_beta(model) %>% group_by(topic) %>% top_n(10, beta) %>% 
  arrange(topic, -beta) %>% mutate( rank = row_number()) %>% 
  ungroup() %>%
  select(-beta) %>% 
  spread(rank, term)
str(model)

topicmodels::build_graph(model, 0)

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
  topics = c(seq(from = 2, to = 3, by = 1)),
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


### look at http://www.bernhardlearns.com/2017/05/topic-models-lda-and-ctm-in-r-with.html for ctm explanation
library(topicmodels)

control_list_ctm <- list(
  seed = 5:9,
  nstart = 5,
  best = TRUE
)
para <- tibble(k = c(5:9))

system.time(
  lemma_tm <- para %>%
    mutate(ctm = map(k, 
                     function(k) CTM(
                       k=k, 
                       x=dtm,
                       control=control_list_ctm
                     )
    )
    )
)
