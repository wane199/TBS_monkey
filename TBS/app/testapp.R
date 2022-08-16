library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("stockVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.

        Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale",
                    value = FALSE),
      
      checkboxInput("adjust",
                    "Adjust prices for inflation", value = FALSE)
    ),
    
    mainPanel(plotOutput("plot")
      )    
  )
)

source("./stockVis/helpers.R") #读R脚本文件
library(quantmod)


# Define server logic required to draw a histogram
server <- function(input, output){ 
  dataInput <- reactive({  
    getSymbols(input$symb, src = "Baidu", 
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)







