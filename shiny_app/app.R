library(shiny)
library(readr)
library(dplyr)
library(kableExtra)
library(writexl)
library(tibble)
library(PORT)

example_correlation_matrix <- read_csv("example_correlation_matrix.csv")

# Define UI for application
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
  #titlePanel(title = span(img(src = "logo.png", height = 90), "Positive-Definiteness Of (Pearson's) r Tables")),
  titlePanel(title = span("Positive-Definiteness Of (Pearson's) r Tables")),
  tags$h3("A method for checking whether correlation tables contain errors & determining which specific correlation may be erroneous"),
  tags$h4("Hussey, Norwood, Cummins, Arslan, & Elson (2024)"),
    
  # Sidebar for inputs: File upload and Download
  sidebarLayout(
    sidebarPanel(
      downloadButton("downloadExample", "Download Example CSV"), 
      tags$hr(),
      fileInput("file1", "Choose CSV or Excel File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xlsx")),
      tags$hr(),
      downloadButton("downloadData", "Download Results")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Test Results", 
                 tags$hr(),
                 tableOutput("testResults")
        ),
        tabPanel("Correlation Imputation", 
                 tags$hr(),
                 textOutput("imputeDescription"),  # This will display the string
                 tags$hr(),
                 tableOutput("dataTable")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive value for storing data
  dataInput <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    if (ext == "csv") {
      read_csv(inFile$datapath, show_col_types = FALSE)
    } else if (ext == "xlsx") {
      readxl::read_excel(inFile$datapath)
    }
  })
  
  # Convert uploaded data
  processedData <- reactive({
    req(dataInput())
    data <- dataInput() |> 
      janitor::clean_names() |> 
      triangle_to_cor_matrix()
    check_correlation_matrix(data)
  })
  
  # Output the original and imputed correlation matrices
  output$dataTable <- renderTable({
    req(processedData())
    processedData()$formatted_results
  })
  
  # Output test results
  output$testResults <- renderTable({
    req(processedData())
    data.frame(test = c("All correlations within bounds [-1, 1]",
                        "Matrix is positive-definite"),
               result = c(processedData()$original_is_within_bounds,
                          processedData()$original_is_pd))
  })
  
  # Downloadable xlsx file of results
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cor_check_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(processedData())
      res <- processedData()
      write_xlsx(list(original = as.data.frame(res$original_matrix),
                      tests = data.frame(is_positive_definite = res$original_is_pd,
                                         does_not_violate_bounds = res$original_is_within_bounds),
                      imputed = as.data.frame(res$imputed_matrix),
                      difference = as.data.frame(res$difference_matrix),
                      combined = as.data.frame(res$formatted_results)),
                 file)
    }
  )
  
  # Download handler for the preformatted CSV file
  output$downloadExample <- downloadHandler(
    filename = function() {
      "example_correlation_matrix.csv"
    },
    content = function(file) {
      write.csv(example_correlation_matrix, file, row.names = FALSE)
    }
  )
  
  # Output for descriptive text in the Correlation Imputation tab
  output$imputeDescription <- renderText({
    "This table displays the reported correlations. The difference between each reported correlation and its imputed value is shown in square brackets. E.g., if imputation suggests that a given correlation may be 0.30 larger than the reported value, this is displayed as [+0.30]. The three correlations with the largest absolute differences are annotated ***, **, and * in decending order."
  })
}

# Run the application
shinyApp(ui = ui, server = server)
