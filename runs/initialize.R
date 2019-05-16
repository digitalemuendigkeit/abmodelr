# setup options
set.seed(0)

# source functions
source(here::here("runs", "helpers.R"))

# create dialogue to select project folder
proj_folder <- rstudioapi::selectDirectory(
  caption = "Select Project Directory", 
  path = here::here("runs", "projects")
)

# select base name of path as project name
project_name <- basename(proj_folder)

# number of YAML files (= number of conditions)
n_conditions <- length(
  list.files(
    here::here("runs", "projects", project_name, "yaml_setup")
  )
)

# create dialogue to select number of simulation iterations
iters <- rstudioapi::showPrompt("How many iterations", "How many iterations do you want to generate", default = NULL)

# create dialogue to select dropbox directory
dropbox_folder <- rstudioapi::selectDirectory(caption = "Select Dropbox Directory", path="~")

# initialize run
initialize_project(
  n_iterations = iters,
  n_conditions = n_conditions,
  project_name = project_name,
  rndm_seeds = rndm_seeds,
  # copy_location = dropbox_folder
)
