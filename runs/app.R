library(shiny)

ui <- navbarPage(
  title = "Create Experiment",
  tabPanel(
    title = "Setup",
    h4("Project Setup"), br(),
    fluidRow(
      column(3,
        textInput(
          "directory name for YAML files", inputId = "dir_name")
      )
    )
  ),
  tabPanel(
    title = "Set Parameters",
    h4("General Settings"),
    br(),
    fluidRow(
      column(3, 
        textInput(
          "project title", inputId = "title")
      ),
      column(3, 
        textInput(
          "output file name (no spaces!)", inputId = "output_file_name")
      )
    ),
    hr(),
    h4("Setup Parameters"), 
    br(),
    fluidRow(
      column(3,
        sliderInput(
          "number of users", inputId = "n_users", 
          min = 10, max = 2000, value = 10, step = 1)
        ),
      column(3, 
        sliderInput(
          "number of topics", inputId = "n_topics", 
          min = 2, max = 30, value = 2, step = 1)
        ),
      column(3, 
        sliderInput(
          "number of initial news posts", inputId = "n_newsposts", 
          min = 10, max = 3000, value = 10, step = 1)      
      ),
      column(3,
        sliderInput(
          "topic limit", inputId = "topic_limit", 
          min = 3, max = 30, value = 3, step = 1)
      )
    ), 
    hr(),
    h4("Simulation Parameters"), 
    br(),
    fluidRow(
      column(3,
        sliderInput(
          "number of simulation steps", inputId = "n_steps", 
          min = 30, max = 200, value = 30, step = 1)      
      ),
      column(3,
        sliderInput(
          "number of newsposts created each steps", inputId = "n_newsposts_step", 
          min = 5, max = 10, value = 5, step = 1)        
      ),
      column(3,
        sliderInput(
          "decay factor", inputId = "decay_factor", 
          min = 0, max = 1, value = 0.5, step = 0.01)
      )
    ),
    br(),
    fluidRow(
      column(3,
             radioButtons(
               "update for user?", inputId = "update_for_user", 
               choices = c("true", "false"))
      ),
      column(3,
             radioButtons(
               "type of recommender algorithm", inputId = "recommender", 
               choices = c("UBCF", "IBCF", "POPULAR", "RANDOM", "RERECOMMEND", "SVD"))  
      )
    ),
    hr(),
    h4("User Interest Update Parameters"),
    br(),
    fluidRow(
      column(3,
        radioButtons(
          "user interest update method", inputId = "update_user_interest",
          choices = c("none", "random", "dominant")
        )
      ),
      column(3,
        sliderInput(
          "probability for user update", inputId = "p_user_update",
          min = 0, max = 1, value = 0.5, step = 0.01
        )
      )
    )
  ),
  tabPanel(
    title = "Review Settings",
    h2("Are all the settings correct?"),
    fluidRow(
      verbatimTextOutput("yaml_test")
    ), 
    hr(), 
    h4("Save as:"),
    p("(\".yml\" file ending will be added automatically)"),
    fluidRow(
      column(3, 
        textInput(
          "(no spaces!)", inputId = "yaml_name")
      )
    ),
    fluidRow(
      column(3, 
        actionButton(
          "save", inputId = "save") 
      )
    ),
    br(),
    p("If you want to create another run setup for this project, change the settings 
      in the \"Set Parameters\" tab and proceed by saving again"),
    br(), br(), br()
  )
)


server <- function(input, output, session) {
  
  output$yaml_test <- renderText({
    paste0(
      "---", "\n",
      "title: ", "\"", toString(input$title), "\"", "\n",
      "n_topics: ", toString(input$n_topics), "\n",
      "n_users: ", toString(input$n_users), "\n",
      "n_newsposts: ", toString(input$n_newsposts), "\n",
      "n_newsposts_step: ", toString(input$n_newsposts_step), "\n",
      "topic_limit: ", toString(input$topic_limit), "\n",
      "decay_factor: ", toString(input$decay_factor), "\n",
      "update_for_user: ", toString(input$update_for_user), "\n",
      "update_user_interest: ", "\"", toString(input$update_user_interest), "\"", "\n",
      "p_user_update: ", toString(input$p_user_update), "\n",
      "n_steps: ", toString(input$n_steps), "\n",
      "outputfilename: ", "\"", toString(input$output_file_name), ".rds\"", "\n",
      "recommender: ", "\"", toString(input$recommender), "\"", "\n",
      "---"
    )
  })

  observeEvent(
    input$save, 
    {
      # directory and file names
      dirname <- toString(input$dir_name)
      if (!is.element(list.dirs(here::here("runs")), here::here("runs", dirname))) {
        dir.create(here::here("runs", dirname))
      }  # test if directory already exists before creating it
      filename <- here::here("runs", dirname, paste0(toString(input$yaml_name), ".yml"))
      
      # write YAML file
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
          "update_user_interest: ", "\"", toString(input$update_user_interest), "\"", "\n",
          "p_user_update: ", toString(input$p_user_update), "\n",
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