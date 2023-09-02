# Load R packages/libraries
library(shiny)
library(shinythemes)
library(RCurl)
library(ggplot2)

tbs <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/TBS_monkey/master/TBS/app/data/M_1018.csv"))


# Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("journal"),
  
  # App title ----
  titlePanel("South China TBS app from JNU"),
  
  # Sidebar layout with a input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "si", label = 'Select x axis', choices = colnames(tbs)[c(-1)]),
      selectInput(inputId = "si1", label = 'Select y axis', choices = colnames(tbs)[c(-1)])
  ),

    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = 'scatterplot')
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
          
  output$scatterplot <- renderPlot({
    qplot(x = tbs[[input$si]], y = tbs[[input$si1]])
  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

