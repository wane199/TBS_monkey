if (interactive()) {
  library(shiny)
  library(ECharts2Shiny)
  
  
  # Server function -------------------------------------------
  server <- function(input, output) {
    # Call functions from ECharts2Shiny to render charts
    renderGauge(div_id = "test",rate = 69, gauge_name = "Finish Rate")
  }
  
  # UI layout -------------------------------------------------
  ui <- fluidPage(
    # We MUST load the ECharts javascript library in advance
    loadEChartsLibrary(),
    
    tags$div(id="test", style="width:50%;height:400px;"),
    deliverChart(div_id = "test")
  )
  
  # Run the application --------------------------------------
  shinyApp(ui = ui, server = server)
}




library(shiny)

myGauge <- function(id, label, value) {
  tagList(
    tags$label(
      `for` = id,
      label
    ),
    tags$meter(
      id = id,
      value = value
    )
  )
}

ui <- fluidPage(
  myGauge("test", "test",0.9)
)

server <- function(input, output, session) {}

shinyApp(ui, server)


