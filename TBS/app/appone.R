# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Load R packages/libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
dt <- fread("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv")
dt <- dt[,-1]

# Build model
model <- randomForest(Age ~ ., data = dt[-1], ntree = 500, mtry = 4, importance = T )

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
# model <- readRDS("model.rds")

# User interface
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),

  # Application title  
  titlePanel("South China TBS app from JNU"),
  # Page header
  headerPanel("Age?"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      HTML("<h3>Input parameters</h3>"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 80,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tags$label(h3('Status/Output')),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should
  #     re-execute automatically when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    x    <- dt[ , 1]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = 'black',
         xlab = "level",
         main = "Histogram of level")
  })
}

# Run the application, create shiny app
shinyApp(ui = ui, server = server)
