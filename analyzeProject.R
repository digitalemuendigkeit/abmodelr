library(tidyverse)
library(data.table)
library(yaml)
library(purrr)
library(rstudioapi)
library(magrittr)

n_condition <- 9
n_iter <- 10
project_name <- "Default Project"
#file_list <- dir(here::here("runs", "projects", project_name, "results"))

# Setup list for n conditions
data <- vector("list", n_condition)

# Loop over conditions
for(j in 1:n_condition) {
  # setup empty list for later index-based accesss
  data[[j]] <- list()
  # loop over iterations
  for (i in 1:n_iter) {
    data[[j]][[i]] <- read_rds(here::here("runs", "projects", project_name, "results", 
                                     paste0(j, "-", i, "-", "output.rds")))
  
  }
}

# add iter-columne function
mut <- function(x, i){
  x %>% mutate(iter = i[[1]], condition = i[[2]])
}


all_iterations <- NULL
for(j in 1:n_condition) {
  for (i in 1:n_iter) {
    #j <- 1
    #i <-1
    temp <- data[[j]][[i]]$user_record
    tmp <- map2(.x= temp, .f = mut, .y = list(rep(list(i,j), 200)))
    all_iterations <- bind_rows(all_iterations, tmp) 
  }
}
all_iterations %<>% rename(mean_value = mean) 


all_iterations %>% 
  group_by(rowname, step, condition) %>% 
  summarise(mean_val = mean(mean_value), se = mean(se)) %>%
  ggplot() +
  aes(x = step, y = mean_val, color = rowname, ymin = mean_val - se, ymax = mean_val + se) +
  geom_errorbar() +
  facet_wrap(~condition, scales = "free_y")


df %>% group_by(step) %>% 
  summarise(polarization = max(mean) - min(mean)) %>% 
  ggplot() +
  aes(x = step, y = polarization) +
  geom_point()

