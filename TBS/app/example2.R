# Load R packages/libraries
library(shiny)
library(shinythemes)
library(RCurl)

tbs <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv"))

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel(textOutput('ti')),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "tbs[,]",
                  label = "Choose a dataset:",
                  choices = c("Age", "BMI", "TBSL1L4")),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),
      
      textInput(inputId = 'title', label = 'title', value = 'shiny') 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Verbatim text for data summary ----
      verbatimTextOutput("summary"),
      
      # Output: HTML table with requested number of observations ----
      tableOutput("view")
      
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  # Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dt,
           "Age" = Age,
           "BMI" = BMI,
           "TBSL1L4" = TBSL1L4)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dt)
  })
  
  # Show the first "n" observations ----
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  output$ti <- renderText(input$title)
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


