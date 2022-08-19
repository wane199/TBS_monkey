## app.R ##
library(shinydashboard)
library(shinythemes)
library(RCurl)

tbs <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv"))

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 20, 80, 30)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
 )

server <- function(input, output) {

  output$plot1 <- renderPlot({
    x    <- tbs$Age
    bins <- seq(min(x), max(x), length.out = input$slider + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Age",
         main = "Histogram of Age Distribution")
  })
}

shinyApp(ui, server)


