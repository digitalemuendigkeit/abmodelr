con_def <- file(here::here('runs', 'init.R'), 'r+')
Lines <- readLines(con_def)

write(
  paste0(
    c("Erste Zeile"), "\n"
  ),
  file = here::here("runs", "bla.R")
)

write(
  Lines,
  file = here::here("runs", "bla.R"),
  append = TRUE
)


close(con_def)
