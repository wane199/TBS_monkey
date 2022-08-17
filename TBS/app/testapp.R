# Import libraries
library(shiny)
library(shinythemes)
library(RCurl)
library(data.table)
library(randomForest)

# Read in the RF model
dt <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv"))

# model <- readRDS("model.rds")

# User interface

ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("South China TBS app from JNU"),# Application title
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



# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(dt), "Age")

pageWithSidebar(
  headerPanel('TBS k-means clustering'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', vars),
    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



