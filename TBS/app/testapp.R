# Import libraries
library(shiny)
library(shinythemes)
library(RCurl)
library(data.table)
library(randomForest)

# Read in the RF model
dt <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv"))

# Read in the RF model
# model <- readRDS("model.rds")

# User interface

ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("South China TBS app from JNU"), # Application title
  headerPanel("Age?"), # Page header
  textInput("name", "What's your name?"),
  textOutput("greeting")
)

server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello ", input$name, "!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
