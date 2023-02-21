# Multi-page {shiny} Applications with {brochure
# https://colinfay.me/brochure-r-package/

# Load R packages/libraries
library(shiny)
library(shinythemes)
library(RCurl)

dt <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/TBS_monkey/master/TBS/app/data/M_1018.csv"))


# Define UI for app that draws a histogram ----
ui <- fluidPage(theme = shinytheme("journal"),
  
  # App title ----
  titlePanel("South China TBS app from JNU"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- dt$Age
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Histogram of Age Distribution")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

