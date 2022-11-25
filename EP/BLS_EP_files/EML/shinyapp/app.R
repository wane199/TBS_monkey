# Osa 1 alku 
 # 
 # This is a Shiny web application. You can run the application by clicking 
 # the 'Run App' button above. 
 # 
 # Find out more about building applications with Shiny here: 
 # 
 #    http://shiny.rstudio.com/ 
 # 
 library(shiny) 
 library(ggplot2) 
 library(DT) 
 library(survival) 
 library(splines) 
 library(shinythemes) 
 library(ggthemes) 
 library(Epi) 
 # Load list of models 
 load(file="models.RData") 
 lv.malli.value<-as.character(seq(tmp.mallit)) 
 names(lv.malli.value)<-sapply(tmp.mallit,function(x){as.character(formula(x))[3]}) 
 tmp.dt.AIC<-data.frame(Model=sapply(tmp.mallit,function(x){as.character(formula(x))[3]}),
                                      AIC=sapply(tmp.mallit,function(x)AIC(x))) 
 tmp.dt.AIC<-tmp.dt.AIC[order(tmp.dt.AIC$AIC),] 
 tmp.dt.AIC$Relative.likelihood<-exp((tmp.dt.AIC$AIC[1]-tmp.dt.AIC$AIC)/2) 
 # Time variable name 
 lv.i.coxph<-sum(class(tmp.mallit[[1]])%in%'coxph') 
 lv.time<-gsub(x=strsplit(x=strsplit(x=as.character(formula(tmp.mallit[[1]]))[2],split = "\\(")[[1]][2],
                          split=",")[[1]][1],pattern = "\\)",replacement = "") 
 # X-variable names 
 tmp.otsikko<-names(tmp.mallit) 
 if(is.null(tmp.otsikko)){Models.otsikko<-paste0("Model ",seq(length(tmp.mallit)))} 
 if(!is.null(tmp.otsikko)){Models.otsikko<-ifelse(tmp.otsikko=="",paste0("Model ",seq(length(tmp.mallit))),tmp.otsikko)} 
 # Osa 1 loppu 
lv.xvars<-c("original_gldm_DependenceEntropy", "log.sigma.5.0.mm.3D_firstorder_Energy", 
"wavelet.LHL_glrlm_GrayLevelNonUniformity")
# Osa 1A alku 
 # Data frame containing predictions 
 tee.dt<-function(input){ 
     lv.dt<-expand.grid( 
 # Osa 1A loppu 
original_gldm_DependenceEntropy=factor(input$original_gldm_DependenceEntropy,levels=c("Low","High")),
log.sigma.5.0.mm.3D_firstorder_Energy=factor(input$log.sigma.5.0.mm.3D_firstorder_Energy,levels=c("Low","High")),
wavelet.LHL_glrlm_GrayLevelNonUniformity=factor(input$wavelet.LHL_glrlm_GrayLevelNonUniformity,levels=c("Low","High"))# Osa 2 alku 
     ) 
     lv.class<-class(tmp.mallit[[as.numeric(input$Model)]]) 
     if(sum(lv.class%in%"coxph")>0){ 
         lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type="survival") 
     } 
     if(sum(lv.class%in%c("lm","glm"))>0){lv.pred<-predict(tmp.mallit[[as.numeric(input$Model)]],newdata=lv.dt,se.fit = TRUE,type="response")} 
     lv.dt$Predicted<-lv.pred$fit 
     lv.dt$se.fit<-lv.pred$se.fit 
     lv.dt$Predicted.lo<-lv.pred$fit-1.96*lv.pred$se.fit 
     lv.dt$Predicted.hi<-lv.pred$fit+1.96*lv.pred$se.fit 
     if(sum(lv.class%in%"coxph")>0){ 
          lv.dt$Predicted.lo<-ifelse(lv.dt$Predicted.lo<0,0,lv.dt$Predicted.lo) 
      } 
     lv.dt 
 } 
 # Define UI for application that draws a histogram 
 ui <- fluidPage(theme = shinytheme( 
 "cerulean"), 
     # Application title 
     titlePanel( 
 "Predicting TLE probability"), 
     # Sidebar with a slider input 
     sidebarLayout( 
         sidebarPanel( 
             textAreaInput("AddPlot",label="Add plot script",value="",rows=3), 
             actionButton("Toteuta", label="Submit"), 
             radioButtons("Model",label="Select model", 
                              choices=lv.malli.value,inline=FALSE), 
             # Osa 2 loppu 
checkboxGroupInput(inputId="original_gldm_DependenceEntropy", label="original_gldm_DependenceEntropy",
 choices=c("Low"="Low","High"="High"), selected = "Low"),
checkboxGroupInput(inputId="log.sigma.5.0.mm.3D_firstorder_Energy", label="log.sigma.5.0.mm.3D_firstorder_Energy",
 choices=c("Low"="Low","High"="High"), selected = "Low"),
checkboxGroupInput(inputId="wavelet.LHL_glrlm_GrayLevelNonUniformity", label="wavelet.LHL_glrlm_GrayLevelNonUniformity",
 choices=c("Low"="Low","High"="High"), selected = "Low"),            # Osa 3 alku 
         ), 
         mainPanel( 
             tabsetPanel(id='tabs', 
                 tabPanel("Plot", list(radioButtons("Xvar",label="Select x-variable", 
                                                     choices=lv.xvars,inline=TRUE,selected=ifelse(lv.i.coxph,lv.time,lv.xvars[1])), 
                                        plotOutput("distPlot"), 
             sliderInput(inputId="PredN",label="Number of points", min =10,max =200 ,value =10))), 
                 tabPanel("Data", dataTableOutput("Data")), 
                tabPanel("Summary", verbatimTextOutput("Summary")), 
                 tabPanel("AIC", htmlOutput("AICTab")), 
                 tabPanel("Info",htmlOutput("Info")) 
         ) 
     ) 
 )) 
 # Define server logic required to draw a histogram 
 server <- function(input, output, session) { 
  
     output$distPlot <- renderPlot({ 
         tmp.dt<-tee.dt(input) 
         lv.group<-sapply(tmp.dt,function(x){is.factor(x)&(length(unique(x))>1)}) 
         lv.group<-names(lv.group)[lv.group] 
         if(!is.factor(tmp.dt[[input$Xvar]])&length(lv.group)>0){ 
             lv.txt<-paste("lv.p1<-ggplot(tmp.dt,aes(x=",input$Xvar, 
                           ",y=Predicted,group=",lv.group[1],"))+geom_line(aes(linetype=",lv.group[1],"))") 
         } 
         else {lv.txt<-paste("lv.p1<-ggplot(tmp.dt,aes(x=",input$Xvar,",y=Predicted))+geom_line()")} 
         eval(parse(text=lv.txt)) 
         if(is.factor(tmp.dt[[input$Xvar]])) lv.p1<-lv.p1+geom_point() 
         if(is.factor(tmp.dt[[input$Xvar]])){lv.p1<-lv.p1+geom_errorbar(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi),width = 0.2)} 
         else {lv.p1<-lv.p1+geom_ribbon(aes(ymin = tmp.dt$Predicted.lo, ymax = tmp.dt$Predicted.hi), alpha=.2)} 
         input$Toteuta 
         isolate(eval(parse(text=paste("lv.p1<-lv.p1",input$AddPlot,collapse="")))) 
         lv.p1 
     }) 
     output$Data<-DT::renderDataTable({ 
         tmp.dt<-tee.dt(input) 
     tmp.title<-'Data and predictions' 
 DT::datatable(tmp.dt,
                           extensions = 'Buttons',escape=TRUE,
                           options = list(
                             pageLength = 50,
                             dom = 'Blfrtip',
                             buttons = list(
                               list(extend = 'copy', title = tmp.title),
                               list(extend = 'csv', title = tmp.title),
                               list(extend = 'excel', title = tmp.title),
                               list(extend = 'pdf', title = tmp.title)
                             )
                           )) 
     }) 
     output$AICTab<- renderPrint({ 
       knitr::kable(tmp.dt.AIC,format='html',caption='Compring models with AIC') 
      }) 
     output$Summary<- renderPrint({ 
      if(sum(class(tmp.mallit[[as.numeric(input$Model)]])%in%"coxph")>0)lv.testi<-cox.zph(tmp.mallit[[as.numeric(input$Model)]]) 
     else{lv.testi<-NULL} 
         tmp.ulos.S<-list(summary(tmp.mallit[[as.numeric(input$Model)]]),lv.testi) 
          if(is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','')} 
          if(!is.null(lv.testi)) {names(tmp.ulos.S)<-c('Model','Testing PH assumption')} 
         cat("\n Current model:" ,Models.otsikko[as.numeric(input$Model)],"\n\n") 
         tmp.ulos.S 
     }) 
     output$Info<-renderText({ 
          HTML( scan(quiet=TRUE,file="ModelInfo.html",what=""))}) 
 } 
 # Run the application 
 shinyApp(ui = ui, server = server) 
