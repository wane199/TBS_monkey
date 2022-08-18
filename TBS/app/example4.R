# Boxplot
# Load R packages/libraries
library(shiny)
library(shinythemes)
library(RCurl)

tbs <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/Presentation/master/TBS/app/data/M_1018.csv"))

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
# tbs$ <- factor(tbs$, labels = c("Automatic", "Manual"))


# Define UI for miles per gallon app ----
ui <- fluidPage(theme = shinytheme("yeti"),
  
  # App title ----
  titlePanel("South China TBS app from JNU"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      HTML("<h3>Input parameters</h4>"),
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Age" = "Age",
                    "BMI" = "BMI",
                    "BMDL1L4" = "BMDL1L4",
                    "TscoreL1L4" = "TscoreL1L4")),

      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Header + summary of distribution ----
      h3("Summary"),
      verbatimTextOutput("summary"),
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("TBSL1L4 ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  output$summary <- renderPrint({
    summary(tbs)
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = tbs,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

