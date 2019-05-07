source(here::here("runs", "helpers.R"))
set.seed(0)

project_name <- "test-project"

# create results directory
dir.create(here::here("runs", "projects", toString(project_name), "results"))

config_length <- list.files(
  here::here("runs", "projects", project_name, "yaml_setup")
)
rndm_seeds <- abs(round(rnorm(300) * 100, 0))

for (i in 1:length(config_length)) {
  
  # index of condition
  cond <- i
  
  # path
  path <- here::here(
    "runs", "projects", project_name, paste0("cond-", toString(i), ".R")
  )
  
  # open connection to template file
  con_tmpl <- file(here::here("runs", "template.R"), "r+")
  lines_tmpl <- readLines(con_tmpl)
  
  # write libraries
  write(
    lines_tmpl[1:11],
    file = path
  )

  # write loading of config file
  write(
    paste0(
      "config <- load_config(\"", project_name, 
      "\")[[\"", "cond-", toString(i), ".yml", "\"]]", "\n"
    ),
    file = path,
    append = TRUE
  )
  
  # write setting of random seed
  write(
    lines_tmpl[14],
    file = path,
    append = TRUE
  )
  
  write(
    paste0(
      "set.seed(", toString(rndm_seeds[1]), ")", "\n"
    ),
    file = path,
    append = TRUE
  )
  
  # write body of script
  write(
    lines_tmpl[16:194],
    file = path,
    append = TRUE
  )
  
  # write file saving
  save_path <- here::here("runs", "projects", toString(project_name), "results")
  
  write(
    paste0(
      "rds_filename <- paste0(\"", toString(cond), "\", \"-\", ",  "config$outputfilename)", "\n",
      "write_rds(", "path = here::here(", 
      "\"runs\", \"projects\", \"", toString(project_name), 
      "\", \"results\"), x = results_data)", "\n"
    ),
    file = path,
    append = TRUE
  )
  
  close(con_tmpl)
  
}


