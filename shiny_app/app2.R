
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(qgraph)
library(PORT)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                  body {
                    background-image: url('white_wave.png');
                    background-repeat: repeat;
                  }
                  h2 {
                    color: #123095;
                  }
                "))
  ),
  # titlePanel(title = span(img(src = "logo.png", height = 90), "Positive-definiteness Of (Pearson's) r Tables")),
  titlePanel(title = span("Positive-definiteness Of (Pearson's) r Tables")),
  tags$h3("A method for calculating the possible bounds of a correlation (r_AB) given two others (r_AC, r_BC)"),
  tags$h4("Hussey, Norwood, Cummins, Arslan, & Elson (2024)"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rAB", "Reported correlation between A and B:", value =  0.5, min = -1, max = 1, step = 0.05),
      numericInput("rBC", "Reported correlation between B and C:", value = -0.1, min = -1, max = 1, step = 0.05),
      numericInput("rAC", "Reported correlation between A and C:", value =  0.1, min = -1, max = 1, step = 0.05)
    ),
    mainPanel(
      tableOutput("results_table"),
      plotOutput("network_plot")  
    )
  )
)

# server logic
server <- function(input, output) {
  output$results_table <- renderTable({
    correlation_consistency_trio(
      rAB = input$rAB, 
      rBC = input$rBC, 
      rAC = input$rAC,
      digits = 3
    )
  })
  
  output$network_plot <- renderPlot({
    # create a correlation matrix
    corr_matrix <- matrix(c(1, input$rAB, input$rAC, 
                            input$rAB, 1, input$rBC, 
                            input$rAC, input$rBC, 1), 
                          nrow = 3)
    rownames(corr_matrix) <- colnames(corr_matrix) <- c("A", "B", "C")
    
    # plot using qgraph
    positions <- matrix(c(0, 1, -1, 0, 1, 0), 
                        ncol = 2, 
                        byrow = TRUE) 
    
    qgraph(corr_matrix, 
           layout = positions, 
           vsize = 5, 
           esize = 15)
  })
}

# run the application
shinyApp(ui = ui, server = server)

