# source functions
source(here::here("runs", "helpers.R"))

# setup options
set.seed(0)
project_name <- "test-project"
config_length <- list.files(
  here::here("runs", "projects", project_name, "yaml_setup")
)
rndm_seeds <- abs(round(rnorm(300) * 100, 0))

# initialize run
initialize_project(
  n_iterations = 10,
  n_conditions = length(config_length),
  project_name = project_name,
  rndm_seeds = rndm_seeds
)
