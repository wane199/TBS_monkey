ui = bootstrapPage(fluidPage(theme = shinytheme("yeti"),
                             titlePanel('TLE Relapse Dynamic Nomogram form JNU'),
                             sidebarLayout(sidebarPanel(uiOutput('manySliders'),
                                                        checkboxInput('trans', 'Alpha blending (transparency)', value = TRUE),
                                                        actionButton('add', 'Predict'),
                                                        br(), br(), 
                                                        helpText('Press Quit to exit the application'),
                                                        actionButton('quit', 'Quit'),
                                                        br(),
                                                        img(src = "https://ts1.cn.mm.bing.net/th/id/R-C.c80600d38debc68a12b4b566886c8216?rik=bTkNEfTXK0fisg&riu=http%3a%2f%2fpicture.swwy.com%2fY2UzZDljYTQxNjhmNDI.jpg&ehk=WYS7zLiw1qw9kNUCW14LEMFnE2n0sOPMwjkmxBh71%2fs%3d&risl=&pid=ImgRaw&r=0&sres=1&sresct=1",
                                                            height = 100, width = 180),
                                                        img(src = "https://github.com/wane199/Presentation/blob/master/EP/EP_Cox_Nomo/shinyPredict/999logo.png?raw=true",
                                                            height = 100, width = 180),
                             ),
                             mainPanel(tabsetPanel(id = 'tabs',
                                                   tabPanel('Survival plot', plotOutput('plot')),
                                                   tabPanel('Predicted Survival', plotlyOutput('plot2')),
                                                   tabPanel('Numerical Summary', verbatimTextOutput('data.pred')),
                                                   tabPanel('Model Summary', verbatimTextOutput('summary'))
                             )
                             )
                             )))
