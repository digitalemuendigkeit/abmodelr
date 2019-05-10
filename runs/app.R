library(shiny)
library(shinyBS)

# Messages ----
popup_text_user_update <- paste("<b>None</b>: Users interest do not get updated at all.",
                  "<b>Random</b>: Users interests of all topics the news post contains get updated with a probability on the right.",
                  "<b>Dominant</b>: Only the dominant topic of the news post will be updated in the users interests.", sep = "<br>")
popup_text_update_for_user <- paste("<b>True</b>: Recommendation matrix gets updated every time a user consumes an item.",
                            "<b>False</b>: Recommendation matrix only gets updated once per step.", sep = "<br>")


counter <- 1

# UI Begin ----
ui <- navbarPage(id = "inTabSet",
  title = "Create Experiment",
  # Create Experiment Panel ----
  tabPanel(
    title = "Setup",
    h4("Project Setup"), br(),
    fluidRow(
      column(3,
        textInput(
          "directory name for YAML files", inputId = "dir_name", placeholder = "my_project", value="test")
      )
    ),
    fluidRow(
      column(3, 
        textInput(
          "project title", inputId = "title", placeholder = "A description", value = "")
      )
    ),
    fluidRow(
      column(3, 
        textInput(
          "output file name (no spaces!)", inputId = "output_file_name", value = "output")
      )
    ),
    fluidRow(
      column(12,
             actionButton(inputId = "goto_params", label = "Continue"))
    )
  ),
  
  # Parameters Panel ----
  tabPanel(value = "params",
    title = "Set Parameters",
    h4("Setup Parameters"), actionButton(inputId = "finish", label = "Save Settings"),
    br(),
    fluidRow(
      column(3,
        sliderInput(
          "number of users", inputId = "n_users", 
          min = 10, max = 2000, value = 10, step = 10)
        ),
      column(3, 
        sliderInput(
          "number of topics", inputId = "n_topics", 
          min = 2, max = 30, value = 2, step = 1)
        ),
      column(3, 
        sliderInput(
          "number of initial news posts", inputId = "n_newsposts", 
          min = 10, max = 3000, value = 10, step = 10)      
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
          min = 0, max = 1, value = 0.5, step = 0.01),
        bsTooltip("decay_factor", "The smaller the factor, the faster news posts lose their relevance.",
                  placement = "bottom", trigger = "hover")
      )
    ),
    br(),
    fluidRow(
      column(3,
             radioButtons(
               "update for user?", inputId = "update_for_user", 
               choices = c("true", "false")),
             bsPopover("update_for_user", title = "Options", content = popup_text_update_for_user,
                       placement = "right", trigger = "hover", options = list(container = "body"))
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
        ),
        bsPopover("update_user_interest", title = "Options", content = popup_text_user_update,
                  placement = "right", trigger = "hover", options = list(container = "body"))
      ),
      column(3,
        sliderInput(
          "probability for user update", inputId = "p_user_update",
          min = 0, max = 1, value = 0.5, step = 0.01
        )
      )
    )
  ),
  
  # Create Job Panel ----
  tabPanel(
    title = "Review Settings",
    value = "review",
    h2("Are all the settings correct?"), 
    hr(), 
    h4("Save as:"),
    fluidRow(
      column(3,
             p(textOutput("filemessage")),
             actionButton(
               inputId = "save", label = "Add Condition"),
             br()
      )
    ),
    fluidRow(
      verbatimTextOutput("yaml_test")
    ), 
    fluidRow(
    br(),
    p("If you want to finish this project, close the app!"),
    br(), br(), br()
    )
  )
)

# Server ----
server <- function(input, output, session) {
  values <- reactiveValues(count = 1)
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

  # Buttons
  observeEvent(
    input$goto_params,
    {
      updateTabsetPanel(session, "inTabSet",
                        selected = paste0("params"))
    }
  )

  observeEvent(
    input$finish,
    {
      updateTabsetPanel(session, "inTabSet",
                        selected = paste0("review"))
    }
  )
  
  
  # Render Text file name ----
  output$filemessage <- renderText({
    paste0("Filename: cond-",values$count ,".yml")
  })
  
  # Event Save ----
  observeEvent( input$save, 
    {
      
      # directory and file names
      dirname <- toString(input$dir_name)
      if (
        !is.element(
          list.dirs(here::here("runs", "projects")), 
          here::here("runs", "projects", dirname)
        )
      ) {
        dir.create(here::here("runs", "projects", dirname))
        dir.create(here::here("runs", "projects", dirname, "yaml_setup"))
      }  # test if directory already exists before creating it
      filename <- here::here(
        "runs", "projects", dirname, "yaml_setup", 
        paste0("cond-",values$count, ".yml")
      )
      
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
    

      values$count <- values$count + 1
      message( )
      updateTabsetPanel(session, "inTabSet",
                        selected = paste0("params"))
    }
    
  )
  
}

shinyApp(ui, server)