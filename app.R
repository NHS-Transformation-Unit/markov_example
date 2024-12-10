
# Set-up ------------------------------------------------------------------

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

source("R/markov_model.R")
source("www/themes/tu_ggplot_themes.R")

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  "Markov Modelling Example",
  id = "navbar",
  
  tags$head(
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "themes/tu_theme.css")
  ),
  
  tabPanel(
    "Introduction",
    fluidPage(
      img(src = "images/TU_logo_large.png",
          height = "100px",
          align = "right"),
      h1("Markov Modelling Example Shiny App"),
      br(),
      h3("Markov Modelling Introduction"),
      p("This app has been created to demonstrate how Markov Models work. 
        Markov Models are Markov modelling is a mathematical technique used to 
        model how entities can move between different states over time. 
        It is particularly useful where outcomes are uncertain and the 
        transition between different states can be expressed as a probability. 
        They are frequently used in health economics to model issues such as 
        the spread of an infection or disease progression. The model can 
        simulate how patients move between different ", em("health states")," 
        across multiple time periods or ", em("cycles.")),
      br(),
      h3("How to use the App"),
      p("The app has two pages that can be used to run a Markov Model and then 
        view the results. On the Inputs page you  can specify the number of 
        states within the model, the number of entities (i.e. patients) within 
        each state and the probabity of an entity moving from one state to 
        another. You an also then determine the number of cycles you wish to 
        run the model for. Once the parameters are determined you can run the 
        model to determine the number of entities within each state after each 
        cycle."),
      p("The Outputs page then contains a data table showing the number of 
        entities within each state after each cycle and a chart visualising 
        this."),
      p("There is an example of how the model can be applied to model disease 
        progression. Please see the Example tab for further information."),
      br(),
      h3("How was the App built"),
      p("This app has been created using the R programming language and Shiny. 
        The code for creating the app is available within our GitHub Repository 
        available ",
        a("here.",
          href = "https://github.com/NHS-Transformation-Unit/markov_example"),
          target = "_blank"))
      
      
    
    
    
  ),
  
  tabPanel(
    "Example",
    value = "example",
    fluidPage(
      img(src = "images/TU_logo_large.png",
          height = "100px",
          align = "right"),
      h1("Example Markov Model"),
      br(),
      h3("Simple Disease Progression Model"),
      p("This example uses a simple disease progression model to demonstrate 
        how this app can be used. In this example we want to model disease 
        progression. A patient can be in one of the following four states:",
        tags$ul(
          tags$li("Healthy: Patient is healthy but susceptible to the disease"),
          tags$li("Mild Disease: A patient has a mild form of the disease."),
          tags$li("Severe Disease: A patient has a severe form of the disease 
                  which requires a greater level of healthcare resources to 
                  manage."),
          tags$li("Deceased: The patient is deceased.")
        )),
      p("We then need to consider how a patient can move from one state to 
        another. I.e. can a person with mild disease recover and move back to 
        being healthy? In this example the following movements between health 
        states are possible:",
        tags$ul(
          tags$li("From Healthy: A healthy patient can move contract the disease
                  and move into the Mild Disease state. The healthy patient can 
                  also die (from any cause) so would move into the Deceased 
                  state. Finally it is also possible for a healthy patient to 
                  remain in a healthy state."),
          tags$li("From Mild Disease: A patient with a mild form of the disease 
                  can recover and move back into the Healthy population but they
                  will remain susceptible to the disease in the future. Of 
                  course, the disease can also progress to the Severe state. 
                  A patient with mild disease can also die and move to 
                  the deceased state. Again a patient can also remain in the 
                  same state."),
          tags$li("From Severe Disease: In this model once a patient has a 
                  severe form of the disease it is not possible to recover to 
                  either a mild form or a healthy state. Therefore, a patient 
                  with severe disease can either remain with severe disease or 
                  die and move to the deceased state."),
          tags$li("From Deceased: The deceased state represents an absorption 
                  state. I.e. Once a patient is deceased they remain in this 
                  state.")
        )),
      p("These states and possible movements can be demonstrated in the diagram 
        below:")
    )
  ),
  
  
  tabPanel(
    "Inputs",
    value = "inputs",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          numericInput("num_states",
                       "Number of States:",
                       value = 4,
                       min = 2,
                       max = 8,
                       width = "200px"),
          textInput("state_names",
                    "State Names (seperate with a comma):",
                    value = "Healthy, Mild Disease, Severe Disease, Deceased",
                    width = "400px"),
          textInput("initial_counts",
                    "Initial counts in each state:",
                    value = "15000, 1200, 400, 0",
                    width = "400px"),
          sliderInput("num_cycles",
                      "Number of Cycles:",
                      min = 1,
                      max = 25,
                      value = 10,
                      width = "300px"),
          actionButton("update_matrix",
                       "Update Transition Matrix"),
          actionButton("run_model",
                       "Run Markov Model")
        ),
        mainPanel(
          h3("Transition Matrix"),
          p("The transition matrix can be edited for the probabilities you wish 
            to use within the model. Ensure that each row adds to 1."),
          DTOutput("transition_matrix")
        )
      )
    )
  ),
  
  tabPanel(
    "Outputs",
    value = "outputs",
    fluidPage(
      h3("Markov Model Results"),
      p("The table below shows the number of entities within each stage for 
        each cycle of the model. Cycle 0 represents the initial counts set for 
        each state."),
      downloadButton("download_data", "Download Data as CSV"),
      tableOutput("model_results_table"),
      br(),
      p("The chart below shows this data table in a visual format with the 
        number of entities in each state shown by a different line."),
      downloadButton("download_plot", "Download Chart as PNG"),
      plotOutput("model_plot")
    )
  )
  
)
  
  
  


# Server ------------------------------------------------------------------

server <- function(input, output, session){
 
  transition_matrix <- reactiveVal()
  markov_results <- reactiveVal()
  
  state_names <- reactive({
    strsplit(input$state_names, ",\\s*")[[1]]
  })
  
  initial_counts <- reactive({
    as.numeric(strsplit(input$initial_counts, ",\\s*")[[1]])
  })
  
  observeEvent(input$update_matrix, {
    num_states <- input$num_states
    new_matrix <- matrix(1 / num_states, nrow = num_states, ncol = num_states)
    colnames(new_matrix) <- state_names()
    rownames(new_matrix) <- state_names()
    transition_matrix(new_matrix)
  })
 
  output$transition_matrix <- renderDT({
    req(transition_matrix())
    datatable(transition_matrix(),
              editable = TRUE,
              rownames = TRUE,
              options = list(
                dom = "t",
                paging = FALSE,
                searching = FALSE,
                ordering = FALSE
              )
    )
  })
  
  observeEvent(input$transition_matrix_cell_edit, {
    req(transition_matrix())
    info <- input$transition_matrix_cell_edit
    mat <- transition_matrix()
    mat[info$row, info$col] <- as.numeric(info$value)
    transition_matrix(mat)
  })
  
  observeEvent(input$run_model, {
    req(state_names(), initial_counts(), transition_matrix())
    if (length(state_names()) != input$num_states || length(initial_counts()) != input$num_states) {
      showModal(modalDialog(
        title = "Input Error",
        "The number of states, state names, and starting counts must match.",
        easyClose = TRUE
      ))
      return()
    }
   
    result <- markov_model(
      state_names = state_names(),
      start_states = initial_counts(),
      transition_probs = as.matrix(transition_matrix()),
      cycles = input$num_cycles
    )
    
    markov_results(result)
    
    updateNavbarPage(session, "navbar", selected = "outputs")
    
})
  
  
  output$model_results_table <- renderTable({
    req(markov_results())
    results <- as.data.frame(markov_results())
    results <- cbind(Cycle = 0:(nrow(results) - 1), results)
    results
  }, rownames = FALSE)
  
  output$model_plot <- renderPlot({
    req(markov_results())
    results <- as.data.frame(markov_results())
    results <- cbind(Cycle = 0:(nrow(results) - 1), results)
    results_long <- results |>
      pivot_longer(cols = -(Cycle),
                   names_to = "State",
                   values_to = "Count") |>
      mutate(State = factor(State, levels = state_names()))
    
    ggplot(results_long, aes(x = Cycle, y = Count, color = State, 
                             group = State)) +
      geom_line(linewidth = 0.9) +
      scale_y_continuous(labels = comma) +
      scale_x_continuous(breaks = seq(0, max(results_long$Cycle), by = 2)) + 
      labs(x = "Cycle",
           y = "Count",
           title = "Markov Model Results",
           subtitle = "Counts by State for Each Cycle") +
      theme_tu_white(hex_col = "#407EC9")
    
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("markov_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      results <- as.data.frame(markov_results())
      results <- cbind(Cycle = 0:(nrow(results) - 1), results)
      write.csv(results, file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("markov_results_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      results <- as.data.frame(markov_results())
      results <- cbind(Cycle = 0:(nrow(results) - 1), results)
      results_long <- results |>
        pivot_longer(cols = -(Cycle),
                     names_to = "State",
                     values_to = "Count") |>
        mutate(State = factor(State, levels = state_names()))
      
      print(
        ggplot(results_long, aes(x = Cycle, y = Count,
                                 color = State, group = State)) +
          geom_line(linewidth = 0.9) +
          scale_y_continuous(labels = comma, expand = c(0, 0)) +
          scale_x_continuous(breaks = seq(0, max(results_long$Cycle), by = 2),
                             expand = c(0, 0)) +
          labs(x = "Cycle",
               y = "Count",
               title = "Markov Model Results",
               subtitle = "Counts by State for Each Cycle") +
          theme_minimal()
      )
      dev.off()
    }
  )
  
}

# Run ---------------------------------------------------------------------

shinyApp(ui, server)

