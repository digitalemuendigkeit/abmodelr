source(here::here("runs", "helpers.R"))
set.seed(0)

config_list <- load_config("test-project")
rndm_seeds <- abs(round(rnorm(300) * 100, 0))

for (i in 1:length(config_list)) {
  
  # index of condition
  cond <- i
  
  # open connection to template file
  con_tmpl <- file(here::here("runs", "template.R"), "r+")
  lines_tmpl <- readLines(con_tmpl)
  
  # write libraries
  write(
    lines_tmpl[1:9],
    file = here::here("runs", "bla.R")
  )

  
  
  write(
    lines_tmpl[1:12],
    file = here::here("runs", "bla.R"),
    append = TRUE
  )
  
  
  close(con_tmpl)
  
}


