# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(shinyjs)

server <- function(input, output, session) {
  
  observe({
    # even though the slider is not involved in a calculation, if
    # you change the slider it will run all this code and update the text box
    # changes to the mytext box also will trigger the code to run
    input$myslider
    txt <- paste(input$mytext, sample(1:10000, 1))
    updateTextInput(session, inputId = "myresults", value = txt)  
    
  })
  
}

ui <- basicPage(
  h3("The results text box gets updated if you change the other text box OR the slider."),
  sliderInput("myslider", "A slider:", min=0, max=1000, value=500),
  textInput("mytext", "Input goes here", value = "Initial value"),
  textInput("myresults", "Results will be printed here")
)

shinyApp(ui = ui, server = server)

