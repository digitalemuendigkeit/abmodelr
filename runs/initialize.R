# source functions
source(here::here("runs", "helpers.R"))

# setup options
set.seed(0)

proj_folder <- rstudioapi::selectDirectory(caption = "Select Project Directory", path=here::here("runs", "projects"))
project_name <- basename(proj_folder)
config_length <- list.files(
  here::here("runs", "projects", project_name, "yaml_setup")
)
rndm_seeds <- abs(round(rnorm(300) * 100, 0))

iters <- rstudioapi::showPrompt("How many iterations", "How many iterations do you want to generate", default = NULL)

# initialize run
initialize_project(
  n_iterations = iters,
  n_conditions = length(config_length),
  project_name = project_name,
  rndm_seeds = rndm_seeds
)
