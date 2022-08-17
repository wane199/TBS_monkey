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
data <- fread("https://github.com/wane199/Presentation/blob/master/TBS/app/data/F_3061.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("journal"),
  
  # Application title
  titlePanel("South China TBS app from JNU"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
    ),
    # Show a plot of the generated distribution
    mainPanel(
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
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
  
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'skyblue', border = 'black',
         xlab = "level",
         main = "Histogram of level")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
