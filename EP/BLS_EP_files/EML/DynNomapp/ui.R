ui = bootstrapPage(fluidPage(theme = shinytheme("slate"),
    titlePanel('TLE Prediction App from JNU'),
    sidebarLayout(sidebarPanel(uiOutput('manySliders'),
                               uiOutput('setlimits'),
                               actionButton('add', 'Predict'),
                               br(), br(),
                               helpText('Press Quit to exit the application'),
                               actionButton('quit', 'Quit'),
                               br(), br(),
                               img(src = "https://github.com/wane199/Presentation/blob/master/EP/EP_Cox_Nomo/shinyPredict/PT_Brain_20160714100121_3d.gif?raw=true",
                                   height = 150, width = 180)
    ),
    mainPanel(tabsetPanel(id = 'tabs',
                          tabPanel('Graphical Summary', plotlyOutput('plot')),
                          tabPanel('Numerical Summary', verbatimTextOutput('data.pred')),
                          tabPanel('Model Summary', verbatimTextOutput('summary'))
    )
    )
    )))

