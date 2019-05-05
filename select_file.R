#file <- selectFile(caption = "Select File", label = "Select",
#                   path = getActiveProject(), filter = "All YAML Files (*.yml)",
#                  existing = TRUE)

#config <- read_yaml(file)

config <- read_yaml("config.yml")


load_config <- function(proj) {
  
  file_list <- list.files(here::here("runs", proj))
  yaml_list <- list()
  
  for (i in file_list) {
    yaml_list <- rlist::list.append(yaml_list, read_yaml(here::here("runs", proj, i)))
  }
  
  names(yaml_list) <- file_list
  
  return (yaml_list)
  
}