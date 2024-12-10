
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
      tags$hr(style = "#407EC9"),
      p("This app has been created to demonstrate how Markov Models work. 
        Markov Models are Markov modelling is a mathematical technique used to 
        model how entities can move between different states over time. 
        It is particularly useful where outcomes are uncertain and the 
        transition between different states can be expressed as a probability. 
        They are frequently used in health economics to model issues such as 
        the spread of an infection or disease progression. The model can 
        simulate how patients move between different ", em("health states")," 
        across multiple time periods or ", em("cycles.")),
      div(img(src = "images/simple_markov-Frontpage Example.drawio.png",
              height = "350px"),
          style = "text-align: center;"),
      br(),
      p("For example the diagram above is a representation of a Markov Model 
        showing the relationships between different health states."),
      br(),
      h3("How to use the App"),
      tags$hr(style = "#407EC9"),
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
      tags$hr(style = "#407EC9"),
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
      tags$hr(style = "#407EC9"),
      p("This example uses a simple disease progression model to demonstrate 
        how this app can be used. In this example we want to model disease 
        progression. A patient can be in one of the following four states:",
        tags$ul(
          tags$li(HTML("<strong>Healthy</strong>: Patient is healthy but 
                       susceptible to the disease")),
          tags$li(HTML("<strong>Mild Disease</strong>: A patient has a mild form
                       of the disease.")),
          tags$li(HTML("<strong>Severe Disease</strong>: A patient has a severe 
                        form of the disease which requires a greater level of 
                        healthcare resources to manage.")),
          tags$li(HTML("<strong>Deceased</strong>: The patient is deceased."))
        )),
      p("We then need to consider how a patient can move from one state to 
        another. I.e. can a person with mild disease recover and move back to 
        being healthy? In this example the following movements between health 
        states are possible:",
        tags$ul(
          tags$li(HTML("<strong>From Healthy</strong>: A healthy patient can 
                  move contract the disease and move into the Mild Disease 
                  state. The healthy patient can also die (from any cause) so 
                  would move into the deceased state. Finally it is also 
                  possible for a healthy patient to remain in a healthy 
                  state.")),
          tags$li(HTML("<strong>From Mild Disease</strong>: A patient with a 
                  mild form of the disease can recover and move back into the 
                  Healthy population but they will remain susceptible to the 
                  disease in the future. Of course, the disease can also 
                  progress to the Severe state. A patient with mild disease can 
                  also die and move to the deceased state. Again, a patient can 
                  also remain in the same state.")),
          tags$li(HTML("<strong>From Severe Disease</strong>: In this model 
                  once a patient has a severe form of the disease it is not 
                  possible to recover to either a mild form or a healthy state. 
                  Therefore, a patient with severe disease can either remain 
                  with severe disease or die and move to the deceased state.")),
          tags$li(HTML("<strong>From Deceased</strong>: The deceased state 
                  represents an absorption state. I.e. Once a patient is 
                  deceased they remain in this state."))
        )),
      p("These states and possible movements can be demonstrated in the diagram 
        below:"),
      br(),
      div(img(src = "images/simple_markov-Simple Model - No Probs.drawio.png",
          height = "400px"),
          style = "text-align: center;"),
      br(),
      p("The probabilities for these movements also need to be determined. These
        could be estimated by using evidence from literature reviews, analysis 
        of available datasets or expert opinion. The probabilities represent 
        moving from one state to another during that cycle. Therefore, all 
        probabilities representing how patients move from a state", 
        strong("must sum to 1."), "The diagram below shows this example with 
        these probabilites estimated."),
      br(),
      div(img(src = "images/simple_markov-Simple Model - With Probs.drawio.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      h3("Creating the Inputs"),
      tags$hr(style = "#407EC9"),
      p("We can now add these parameters for our model as inputs so that we can 
        then run the model. This can be done in the following steps.",
        tags$ul(
          tags$li(HTML("<strong>Number of States</strong>: We can amend the 
                  number of states to 4 (Healthy, Mild Disease, Severe Disease, 
                  Deceased). For this model the number of states is limited to 
                  a maximum of 8.")),
          tags$li(HTML("<strong>State names</strong>: We can add the names for 
                  our states as a string of text with each entry separated by a 
                  comma.")),
          tags$li(HTML("<strong>Initial Counts</strong>: This is where we need 
                  to specify how many patients are in each state at the start 
                  of the model. Again, this can be completed as a string of 
                  text with each number separated by a comma.")),
          tags$li(HTML("<strong>Number of cycles</strong>: This is where we can 
                  specify the number of cycles to run. Each cycle represents a 
                  time step within the model so could be any consistent time 
                  unit i.e. a year or a month."))
        )
      ),
      p("We can amend the inputs as shown in the screenshot below to set the 
        correct parameters for our model."),
      div(img(src = "images/screenshot_parameters.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      p(HTML("No we can click on the <strong>Update Transition Matrix</strong> 
             button to create the transition matrix. By default the matrix will 
             populate with values that are all equal to 1 divided by the number 
             of states. Before we edit these probabilities it is important to 
             understand how to read the transition matrix. Each row will be 
             named after each of the states within the model. Each column is 
             also named after each state. Each row representes the state an 
             entity is in at the start of that cycle and the column represnts 
             where they could move to at the end. Therefore, reading across the 
             first row for <strong>Healthy</strong> patients the first column 
             represents the probability they remian in the 
             <strong>Healthy</strong> state. The second column represents the 
             probability of moving to the <strong>Mild Disease</strong> state 
             and so on.")),
      div(img(src = "images/screenshot_transition_matrix_initial.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      p(HTML("We can edit these probabilities to apply the ones from the 
             diagram of the model above. Therefore our first row has the 
             values:"),
        tags$ul(
          tags$li("Healthy -> Healthy = 0.93"),
          tags$li("Healthy -> Mild Disease = 0.04"),
          tags$li("Healthy -> Severe Disease = 0 (as there is no flow between 
                  these stages)"),
          tags$li("Healthy -> Deceased = 0.03")
        )),
      p(HTML("It is important to note that the sum of these probabilities is 
             <strong>equal to 1</strong>. In any version of the model you must 
             ensure that the numbers across each row sum to 1.")),
      p("Therefore our transition matrix should look like the screenshot 
        below:"),
      div(img(src = "images/screenshot_transition_matrix_updated.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      p(HTML("Now we can run the model by clicking the 
             <strong>Run Markov Model</strong> button. This will run the model 
             and take you to the outputs of the model.")),
      br(),
      h3("Understanding the Outputs"),
      tags$hr(style = "#407EC9"),
      p(HTML("The outputs of the model include a table showing the number of 
             patients belonging to each state following each cycle. The first 
             row of the table, Cycle 0, represents the 
             <strong>initial counts</strong> when the model started. You can 
             export this table as a <strong>csv</strong> file using the 
             <strong>Download Data as CSV</strong> button.")),
      br(),
      p(HTML("The second output from the model shows the data contained within 
             the table as a chart. Each line on the chart shows an individual 
             health state, see the legend for more details. The chart can be 
             exported as <strong>png image file</strong> using the 
             <strong>Download Chart as PNG</strong> button.")),
      p(HTML("If you wish to repeat the model with new parameters simply 
             navigate back to the <strong>Inputs</strong> tab within the 
             navigation bar at the top. You can either change the model 
             parameters or amend the transition matrix probabilities. Simply 
             click the <strong>Run Markov Model</strong> button to run the 
             model with the new values."))
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

