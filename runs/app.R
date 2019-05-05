library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      textInput("directory name for YAML files",
        inputId = "dir_name"
      ),
      textInput("project title", 
        inputId = "title"),
      numericInput("number of topics", 
        inputId = "n_topics", 
        min = 2, max = 30, value = 2),
      numericInput("number of users", 
        inputId = "n_users", 
        min = 10, max = 2000, value = 10),
      numericInput("number of news posts", 
        inputId = "n_newsposts", 
        min = 10, max = 3000, value = 10),
      sliderInput("number of steps", 
        inputId = "n_newsposts_step", 
        min = 5, max = 10, step = 1, value = 5),
      numericInput("topic limit", 
        inputId = "topic_limit", 
        min = 3, max = 30, value = 3),
      sliderInput("decay factor", 
        inputId = "decay_factor", 
        min = 0, max = 1, step = 0.01, value = 0.5),
      radioButtons("update for user?", 
        inputId = "update_for_user", 
        choices = c("true", "false")),
      sliderInput("number of simulation steps",
        inputId = "n_steps",
        min = 30, max = 200, value = 30
      ),
      textInput("output file name (no spaces!)", 
        inputId = "output_file_name"),
      radioButtons("type of recommender algorithm", 
        inputId = "recommender", 
        choices = c("UBCF", "IBCF", "POPULAR")),
      textInput("name of YAML file (no spaces!)",
        inputId = "yaml_name"
      ),
      actionButton("save", 
        inputId = "save")
    ),
    mainPanel(
      # TO DO: display control yaml
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(
    input$save, 
    {
      dirname <- toString(input$dir_name)
      dir.create(here::here("runs", dirname))
      filename <- here::here("runs", dirname, paste0(toString(input$yaml_name), ".yml"))
      write(
        "---", 
        file = filename
      )  # begin writing YAML file
      write(
        paste0(
          "title: ", "\"", toString(input$title), "\"", "\n",
          "n_topics: ", toString(input$n_topics), "\n",
          "n_users: ", toString(input$n_users), "\n",
          "n_newsposts: ", toString(input$n_newsposts), "\n",
          "n_newsposts_step: ", toString(input$n_newsposts_step), "\n",
          "topic_limit: ", toString(input$topic_limit), "\n",
          "decay_factor: ", toString(input$decay_factor), "\n",
          "update_for_user: ", toString(input$update_for_user), "\n",
          "n_steps: ", toString(input$n_steps), "\n",
          "outputfilename: ", "\"", toString(input$output_file_name), ".rds\"", "\n",
          "recommender: ", "\"", toString(input$recommender), "\""
        ), 
        file = filename, append = TRUE
      )  # write YAML body
      write(
        "---", 
        file = filename, append = TRUE
      )  # end writing YAML file

    }
    
  )
  
}

shinyApp(ui, server)