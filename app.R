
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
        Markov Models is a mathematical technique used to 
        model how entities can move between different states over time. 
        It is particularly useful where outcomes are uncertain and the 
        transition between different states can be expressed as a probability. 
        This technique is frequently used in health economics to model issues 
        such as the spread of an infection or disease progression. The diagram 
        below shows a few examples of Markov Models:"),
      div(img(src = "images/simple_markov-Frontpage.drawio.png",
              height = "350px"),
          style = "text-align: center;"),
      br(),
      p(HTML("The first example shows a model for a simple model of different 
             weather states. Each type of weather represents a <em>state</em> 
             and the arrows show the possible movements from one day to the 
             next. The second example shows a simple model of disease 
             progression. Here the <em>states</em> are represented by the 
             different conditions that a patient may be in at any given time. 
             There is more information on the key concepts within Markov 
             modelling further on this page.")),
      br(),
      h3("How to use this App"),
      tags$hr(style = "#407EC9"),
      p("This app has four pages that navigate across using the bar at the top 
      of the page. These pages include: ", 
        tags$ul(
          tags$li(HTML("<strong>Introduction (this page)</strong>: The 
                       introduction covers key concepts about Markov Modelling, 
                       important components of the methodology and navigating 
                       the app.")),
          tags$li(HTML("<strong>Example Model</strong>: This page provides an 
                       example of how to re-create a simple disease progression 
                       model within this app. This includes a demonstration of 
                       how to set the model inputs and then how to understand 
                       the outputs of the model.")),
          tags$li(HTML("<strong>Inputs</strong>: This page contains all of the 
                       parameters required to design and run a Markov Model. 
                       These input parameters are explained in more detail in 
                       the next section and within the <strong>Example</strong> 
                       page.")),
          tags$li(HTML("<strong>Outputs</strong>: This page contains the 
                       outputs of the Markov model once it has been run."))
        ),
      br(),
      h3("Key Concepts"),
      tags$hr(style = "#407EC9"),
      h4("1. Entities"),
      p(HTML("Entities represent the subjects being modelled. So in the context 
             of healthcare they would typically represent patients. Entities 
             can move between different <em>states</em> and it is this movement 
             that we want to understand.")),
      h4("2. States"),
      p(HTML("States represent the possible conditions or stages that an 
             entity can belong to. In the healthcare context this could be 
             conditions such as <em>healthy, recovered, or diagnosed</em>.")),
      h4("3. Transitions"),
      p(HTML("Transitions describe the way in which entities move from one 
             state to another. These transitions are represented as the 
             probability of moving to a particular state during a fixed time 
             period i.e. a week or a year.")),
      h4("4. Transition Matrix"),
      p(HTML("A transition matrix is a table that shows the probabilities of 
             moving between states. Each row represents the current state, and 
             each column the probability of transitioning to the new state. The 
             table below provides an example:")),
      p(HTML('
<table style="border: 1px solid black; border-collapse: collapse; width: 25%;" cellpadding="8" cellspacing="0">
  <caption style="color: #407EC9; font-size: 1em;"><strong>Example Transition Matrix</strong></caption>
  <thead>
    <tr>
      <th style="border: 1px solid black;">Current State</th>
      <th style="border: 1px solid black;">Healthy</th>
      <th style="border: 1px solid black;">Unwell</th>
      <th style="border: 1px solid black;">Recovered</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td style="border: 1px solid black;"><strong>Healthy</strong></td>
      <td style="border: 1px solid black;">0.85</td>
      <td style="border: 1px solid black;">0.15</td>
      <td style="border: 1px solid black;">0.00</td>
    </tr>
    <tr>
      <td style="border: 1px solid black;"><strong>Unwell</strong></td>
      <td style="border: 1px solid black;">0.10</td>
      <td style="border: 1px solid black;">0.70</td>
      <td style="border: 1px solid black;">0.20</td>
    </tr>
    <tr>
      <td style="border: 1px solid black;"><strong>Recovered</strong></td>
      <td style="border: 1px solid black;">0.00</td>
      <td style="border: 1px solid black;">0.05</td>
      <td style="border: 1px solid black;">0.95</td>
    </tr>
  </tbody>
</table>
')),
      p(HTML("In this example a patient can belong to one of the three states: 
             healthy, unwell or recovered. Reading the first line of the table 
             for patients in the <em>healthy</em> state, 85% of them will 
             remain within the <em>healthy</em> state, 15% will move to the 
             <em>unwell</em> state and no patients will move to 
             <em>recovered</em> during each cycle. An important point to 
             remember is that the probabilities for each row <strong>must add 
             to 1</strong>. This reflects the fact that, during each cycle, an 
             entity must end up in <em>one</em> of the possible states. For 
             example, in the <strong>Healthy</strong> 
             row (0.85 + 0.15 + 0.00 = 1), the probabilities of remaining 
             healthy or becoming unwell cover all possible outcomes.")),
      h4("5. Cycles"),
      p(HTML("A cycle is the time period over which each of these transitions 
             occur. During each cycle the transition probabilities are applied 
             to the entities within each state. Therefore, at the end of each 
             cycle we can determine how many entities are in each state. The 
             number of cycles within the model is set for the time period the 
             model needs to cover. I.e. if modelling the progression of a 
             disease for five years the number of cycles would be five with each 
             cycle representing one year.")),
      h4("6. Initial Counts"),
      p(HTML("The initial counts represent the number of entities within each 
             state at the beginning of the model. For example, how many 
             patients are in the <em>healthy</em> state when the model 
             begins.")),
      h4("7. Markov Process"),
      p(HTML("By applying the transition matrix for each cycle of this will 
             simulate how entities move between states over time. This enables 
             Markov modelling to be helpful for a range of healthcare modelling 
             scenarios.")),
      br(),
      h3("How was this App built"),
      tags$hr(style = "#407EC9"),
      p("This app has been created using the R programming language and Shiny. 
        The code for creating this app is available within our GitHub Repository 
        available ",
        a("here.",
          href = "https://github.com/NHS-Transformation-Unit/markov_example"),
          target = "_blank"))
      
      
    
    
    ) 
  ),
  
  tabPanel(
    "Example Model",
    value = "example model",
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
        progression over a ten-year period. A patient can be in one of the 
        following four states:",
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
        being healthy? In this example the following movements between the 
        different conditions are possible:",
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
        strong("must sum to 1."), "In this example each cycle is a single year. 
        This means that each probability represents the chance of moving from 
        that condition to another during a year. The diagram below shows this 
        example with these probabilities estimated."),
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
                  of the model. These initial counts would need to be determined 
                  using existing datasets, available literature or clinical 
                  expertise. Again, this can be completed as a string of 
                  text with each number separated by a comma. In this example 
                  analysis of available data has identified that there are 
                  15,000 patients who are healthy but at risk of developing the 
                  disease, 1,200 patients with a mild form of the disease and 
                  400 patients with a severe form. At the start of the model 
                  there would be no deceased patients.")),
          tags$li(HTML("<strong>Number of cycles</strong>: This is where we can 
                  specify the number of cycles to run. Each cycle represents a 
                  time step within the model so could be any consistent time 
                  unit i.e. a year or a month. So, in this case we would set 
                  the number of cycles to 10."))
        )
      ),
      p("We can amend the inputs as shown in the screenshot below to set the 
        correct parameters for our model."),
      div(img(src = "images/screenshot_parametersv2.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      p(HTML("No we can click on the <strong>Update Transition Matrix</strong> 
             button to create the transition matrix. By default, the matrix will 
             populate with values that are all equal to 1 divided by the number 
             of states. These probabilities need to be edited with the 
             probabilities from earlier. Before we edit these probabilities it 
             is important to understand how to read the transition matrix. Each 
             row will be named after each of the states within the model. Each 
             column is also named after each state. Each row represents the 
             state an entity is in at the start of that cycle and the column 
             represents where they could move to at the end. Therefore, reading 
             across the first row for <strong>Healthy</strong> patients the 
             first column represents the probability they remain in the 
             <strong>Healthy</strong> state. The second column represents the 
             probability of moving to the <strong>Mild Disease</strong> state 
             and so on.")),
      div(img(src = "images/screenshot_transition_matrix_initial.png",
              height = "400px"),
          style = "text-align: center;"),
      br(),
      p(HTML("We can edit these probabilities to apply the ones from the 
             diagram of the model above. Therefore, our first row has the 
             values:"),
        tags$ul(
          tags$li(HTML("Healthy &rarr; Healthy = 0.93")),
          tags$li(HTML("Healthy &rarr; Mild Disease = 0.04")),
          tags$li(HTML("Healthy &rarr; Severe Disease = 0 (as there is no flow between 
                  these stages)")),
          tags$li(HTML("Healthy &rarr; Deceased = 0.03"))
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
             <strong>initial counts</strong> when the model started.")),
      div(img(src = "images/markov_results_table_example.png",
              height = "400px"),
          style = "text-align: center;"),
      p(HTML("You can export this table as a <strong>csv</strong> file using the 
             <strong>Download Data as CSV</strong> button.")),
      br(),
      p(HTML("The second output from the model shows the data contained within 
             the table as a chart. Each line on the chart shows an individual 
             health state, see the legend for more details.")),
      div(img(src = "images/markov_results_chart_example.png",
              height = "400px"),
          style = "text-align: center;"),
      p(HTML("The chart can be  exported as <strong>png image file</strong> 
             using the <strong>Download Chart as PNG</strong> button.")),
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
                    "State Names (separate with a comma):",
                    value = "Healthy, Mild Disease, Severe Disease, Deceased",
                    width = "400px"),
          textInput("initial_counts",
                    "Initial counts in each state (separate with a comma):",
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
    
    mat <- as.matrix(transition_matrix())
    
    row_sums <- rowSums(mat)
    if (any(abs(row_sums - 1) > 1e-6)) {
      showModal(modalDialog(
        title = "Transition Matrix Error",
        "Each row in the transition matrix must sum to 1. Please check the probabilities and try again.",
        easyClose = TRUE
      ))
      return()
    }
    
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

