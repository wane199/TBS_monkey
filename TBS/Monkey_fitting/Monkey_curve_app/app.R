# https://shiny.posit.co/r/gallery/
# https://connect.appsilon.com/DepMapV2/
##### Import libraries #####
library(shiny) # Web Application Framework for R
library(shinythemes) # Themes for Shiny
library(RCurl) # General Network (HTTP/FTP/...) Client Interface for R
library(data.table) # Extension of `data.frame`
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(rms) # Regression Modeling Strategies
library(splines)
library(ggpmisc) # Miscellaneous Extensions to 'ggplot2'
library(DT) # CRAN v0.29
library(markdown) # CRAN v1.8
options(digits = 3) # 限定输出小数点后数字的位数

##### Load datasets #####
T1WI <- read.csv("./data/T1_TBV.csv", sep = ";", fileEncoding = "GBK")
PET <- read.csv("./data/PET_SUVr.csv", sep = ";", fileEncoding = "GBK")
# T1WI_csv <- "https://raw.githubusercontent.com/wane199/TBS_monkey/master/TBS/Monkey_fitting/Monkey_curve_app/data/T1_TBV.csv"
# T1WI <- readr::read_csv(T1WI_csv)
# dt <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/TBS_monkey/master/TBS/Monkey_fitting/Monkey_curve_app/data/T1_TBV.csv"))
# df <- read.csv(text = getURL("https://raw.githubusercontent.com/wane199/TBS_monkey/master/TBS/Monkey_fitting/Monkey_curve_app/data/PET_SUVr.csv"))

T1WI$Sex <- as.factor(T1WI$Sex)
T1WI$Age <- as.numeric(T1WI$Age)
PET$Sex <- as.factor(PET$Sex)
PET$Age <- as.numeric(PET$Age)
# data <- list(T1WI, PET)
##### Source helpers #####
source("helpers.R")
##### Read in the RF model #####
# model <- readRDS("model.rds")

##### Define UI for dataset viewer app/User interface #####
ui <- navbarPage("Brain development of the cynomolgus monkey lifespan from JNU",
  theme = shinytheme("cosmo"), # slate united superhero united
  tabPanel(
    "Growth trajectory curves",
    # App title ----
    # titlePanel("Brain development of the cynomolgus monkey lifespan from JNU"), # Application title

    # Sidebar layout with a input and output definitions ----
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        # Input: Selector for choosing dataset ----
        selectInput(inputId = "dataset", label = "Select dataset:", choices = list("T1WI" = "T1WI", "PET" = "PET"), selected = NULL),
        selectInput(inputId = "variable", label = "Select variable:", choices = NULL), # colnames(T1WI)[c(-1, -2)] c("Weight","TBV","TBV.BW","whole","SUVr_whole_refPons","SUV_Whole")
        # Horizontal line ----
        tags$hr(),
        # 添加一个用于输入x值的textInput
        sliderInput(inputId ="xval", label = "Input age (year) of cynomolgus monkey:",
          value = 1, min = 0, max = 30,
          step = 1, animate = T
        ),
        selectInput(inputId = "insex", label = "Input sex (Female/Male) of cynomolgus monkey:", choices = list("F", "M"), selected = NULL),
        # actionButton('add', 'Predict'),
        uiOutput("vy"),
        verbatimTextOutput(outputId = "prediction"),
        withMathJax(helpText('*Unit of the relative parameter:$$\\text{Volume:}cm^3;\\text{Weight:}kg;
                             \\text{TBV.Weight:}cm^3/kg$$')),
        helpText(withMathJax('$$\\text{Uptake Value:}kBq/cc;\\text{SUV:}g/ml;\\text{SUVr_refPons:}1$$')),
        helpText("*Input the age and sex of cynomolgus monkey, the relative parameter of brain will be shown in the box above"),
        tags$hr(),
        br(),
        img(
          src = "https://ts1.cn.mm.bing.net/th/id/R-C.c80600d38debc68a12b4b566886c8216?rik=bTkNEfTXK0fisg&riu=http%3a%2f%2fpicture.swwy.com%2fY2UzZDljYTQxNjhmNDI.jpg&ehk=WYS7zLiw1qw9kNUCW14LEMFnE2n0sOPMwjkmxBh71%2fs%3d&risl=&pid=ImgRaw&r=0&sres=1&sresct=1",
          height = 135, width = 275
        )
      ),

      # Main panel for displaying outputs ----
      mainPanel(
        plotOutput(outputId = "line_plot"),
        verbatimTextOutput("summary")
      )
    )
  ),
  tabPanel(
    "Dataset display",
    DT::dataTableOutput("dis")
  ),
  tabPanel(
    "About",
    titlePanel(""),
    div(includeMarkdown("./data/about.md"),
      align = "justify"
    )
  )
)

##### Define server logic to summarize and view selected dataset #####
server <- function(input, output, session) {
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #     "T1WI" = T1WI,
  #     "PET" = PET)
  # })
  datasetInput <- reactive({
    get(input$dataset)
  })

  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })

  observe({
    var.opts <- colnames(datasetInput()[c(-1, -2)])
    updateSelectInput(session, "variable", choices = var.opts)
  })

  output$vy <- renderText({
    paste("Predicted value of", input$variable, ":")
  })

  output$dis <- DT::renderDataTable(
    DT::datatable(datasetInput(), options = list(pageLength = 25))
  )

  output$line_plot <- renderPlot({
    ggplot(datasetInput(), aes(x = Age, y = .data[[input$variable]], colour = Sex)) +
      geom_point(aes(colour = Sex, shape = Sex), alpha = 1.0, size = 2.5) +
      theme_classic() +
      stat_smooth(method = lm, formula = y ~ rcs(x, 5)) +
      scale_x_continuous(breaks = seq(0, 30, 1)) +
      stat_poly_eq(
        aes(label = paste(after_stat(eq.label), after_stat(adj.rr.label), sep = "~~~~")),
        formula = y ~ rcs(x, 5), parse = TRUE
      ) +
      xlab("Age (year)") +
      ylab(input$variable) +
      theme(
        axis.text = element_text(size = 10, face = "bold"), axis.ticks.length = unit(-0.15, "cm"),
        axis.text.x = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")),
        axis.text.y = element_text(margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
      )
  })


  # sexdata <- subset(datasetInput, Sex==input$insex)
  sexdata <- reactive({
    subset(datasetInput(), get("Sex")==input$insex)
  })

  output$prediction <- renderText({
    # x_var <- "Age"
    # y_var <- input$variable
    # model <- lm(get(y_var) ~ rcs(get("Age"), 5), data = datasetInput())
    model <- lm(formula(paste(input$variable, "~", "rcs", "(", "Age", ",", "df = 5)", sep = "")), data = sexdata()) # datasetInput()
    prediction <- predict(model, newdata = data.frame(Age = input$xval))
    prediction[[1]]
  })
}

##### Create Shiny app #####
shinyApp(ui = ui, server = server)
