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
ui <- fluidPage(theme = shinytheme("lumen"),
  
  # App title ----
  titlePanel("South China TBS app from JNU"),
  navbarPage("TBS app",
             tabPanel("Home",
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      HTML("<h3>Input parameters</h3>"),
      # Input: Selector for variable to plot against mpg ----
      selectInput("variable", "Variable:",
                  c("Age" = "Age",
                    "BMI" = "BMI",
                    "BMDL1L4" = "BMDL1L4",
                    "TscoreL1L4" = "TscoreL1L4")),
      br(),
      img(src = "https://media.springernature.com/original/springer-static/image/art%3A10.1007%2Fs00198-011-1824-6/MediaObjects/198_2011_1824_Fig3_HTML.gif"),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Header + summary of distribution ----
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      # Output: Plot of the requested variable against mpg ----
      plotOutput("boxPlot"),
      h3("Summary"),
      verbatimTextOutput("summary")
      )
    )),

    tabPanel("About",
            titlePanel("About"),
    div(includeMarkdown("./about.md"),
    align="justify")
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
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$boxPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = tbs,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  output$summary <- renderPrint({
    summary(tbs)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

