#file <- selectFile(caption = "Select File", label = "Select",
#                   path = getActiveProject(), filter = "All YAML Files (*.yml)",
#                  existing = TRUE)

#config <- read_yaml(file)

config <- read_yaml("config.yml")


load_config <- function(project_dir) {
  #' read yaml files from a specified project directory
  #' the directory should be located in /runs
  #' 
  #' Args:
  #'   project_dir: directory name within folder "runs"
  #' Returns:
  #'   list of attributes of different runs
  
  # create lists
  file_list <- list.files(here::here("runs", project_dir))
  yaml_list <- list()
  
  # read files and append to list
  for (i in file_list) {
    yaml_list <- rlist::list.append(yaml_list, read_yaml(here::here("runs", project_dir, i)))
  }
  
  names(yaml_list) <- file_list
  
  # return list
  return (yaml_list)
  
}