# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/
# Load R packages/libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
dt <- fread("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv")

# Build model
# model <- randomForest(Age ~ ., data = dt, ntree = 500, mtry = 4, importance = T )

# Save model to RDS file
# saveRDS(model, "model.rds")

# Read in the RF model
# model <- readRDS("model.rds")

# User interface
# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("journal"),

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
        min = 20,
        max = 75,
        value = 30
      ),
      br(),
      img(src = "https://media.springernature.com/original/springer-static/image/art%3A10.1007%2Fs00198-011-1824-6/MediaObjects/198_2011_1824_Fig3_HTML.gif"),
    ),
    # actionButton("Action","Submit"),
    # submitButton("submit"),

    # Show a plot of the generated distribution
    mainPanel(
      h3("Summary"),
      verbatimTextOutput("summary"),
      
      tags$label(h3("Status/Output")),
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should
  #     re-execute automatically when inputs change
  #  2) Its output type is a plot
  output$summary <- renderPrint({
    skimr::skim(dt)
  })
  
  output$distPlot <- renderPlot({
    x <- unlist(dt[, 2]) # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(dt$Age,
      breaks = bins, col = "skyblue", border = "white",
      xlab = "Age",
      main = "Histogram of Age"
    )
  })
}

# Run the application, Create the shiny app
shinyApp(ui = ui, server = server)

