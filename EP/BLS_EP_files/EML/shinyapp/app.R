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
lv.xvars<-c("original_firstorder_Kurtosis", "log.sigma.5.0.mm.3D_glcm_Correlation", 
"wavelet.LLH_firstorder_RootMeanSquared", "wavelet.LLH_glcm_Correlation", 
"wavelet.LLH_glcm_Imc2", "wavelet.LHL_firstorder_Range", "wavelet.LHL_glcm_Autocorrelation", 
"wavelet.LHL_glcm_JointAverage", "wavelet.LHL_glcm_Idn", "wavelet.LHL_glrlm_HighGrayLevelRunEmphasis", 
"wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis", "wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis", 
"wavelet.LHL_glszm_HighGrayLevelZoneEmphasis", "wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis", 
"wavelet.LHL_gldm_HighGrayLevelEmphasis", "wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis", 
"wavelet.LHH_firstorder_Skewness", "wavelet.HLH_glcm_MaximumProbability", 
"wavelet.HLH_glszm_GrayLevelNonUniformity", "wavelet.HLH_glszm_SizeZoneNonUniformityNormalized", 
"wavelet.HLH_glszm_SmallAreaEmphasis", "wavelet.LLL_firstorder_Kurtosis"
)
# Osa 1A alku 
 # Data frame containing predictions 
 tee.dt<-function(input){ 
     lv.dt<-expand.grid( 
 # Osa 1A loppu 
original_firstorder_Kurtosis=seq(from=input$original_firstorder_Kurtosis[1],to=input$original_firstorder_Kurtosis[2],length=ifelse(input$original_firstorder_Kurtosis[1]==input$original_firstorder_Kurtosis[2],1,input$PredN)),
log.sigma.5.0.mm.3D_glcm_Correlation=seq(from=input$log.sigma.5.0.mm.3D_glcm_Correlation[1],to=input$log.sigma.5.0.mm.3D_glcm_Correlation[2],length=ifelse(input$log.sigma.5.0.mm.3D_glcm_Correlation[1]==input$log.sigma.5.0.mm.3D_glcm_Correlation[2],1,input$PredN)),
wavelet.LLH_firstorder_RootMeanSquared=seq(from=input$wavelet.LLH_firstorder_RootMeanSquared[1],to=input$wavelet.LLH_firstorder_RootMeanSquared[2],length=ifelse(input$wavelet.LLH_firstorder_RootMeanSquared[1]==input$wavelet.LLH_firstorder_RootMeanSquared[2],1,input$PredN)),
wavelet.LLH_glcm_Correlation=seq(from=input$wavelet.LLH_glcm_Correlation[1],to=input$wavelet.LLH_glcm_Correlation[2],length=ifelse(input$wavelet.LLH_glcm_Correlation[1]==input$wavelet.LLH_glcm_Correlation[2],1,input$PredN)),
wavelet.LLH_glcm_Imc2=seq(from=input$wavelet.LLH_glcm_Imc2[1],to=input$wavelet.LLH_glcm_Imc2[2],length=ifelse(input$wavelet.LLH_glcm_Imc2[1]==input$wavelet.LLH_glcm_Imc2[2],1,input$PredN)),
wavelet.LHL_firstorder_Range=seq(from=input$wavelet.LHL_firstorder_Range[1],to=input$wavelet.LHL_firstorder_Range[2],length=ifelse(input$wavelet.LHL_firstorder_Range[1]==input$wavelet.LHL_firstorder_Range[2],1,input$PredN)),
wavelet.LHL_glcm_Autocorrelation=seq(from=input$wavelet.LHL_glcm_Autocorrelation[1],to=input$wavelet.LHL_glcm_Autocorrelation[2],length=ifelse(input$wavelet.LHL_glcm_Autocorrelation[1]==input$wavelet.LHL_glcm_Autocorrelation[2],1,input$PredN)),
wavelet.LHL_glcm_JointAverage=seq(from=input$wavelet.LHL_glcm_JointAverage[1],to=input$wavelet.LHL_glcm_JointAverage[2],length=ifelse(input$wavelet.LHL_glcm_JointAverage[1]==input$wavelet.LHL_glcm_JointAverage[2],1,input$PredN)),
wavelet.LHL_glcm_Idn=seq(from=input$wavelet.LHL_glcm_Idn[1],to=input$wavelet.LHL_glcm_Idn[2],length=ifelse(input$wavelet.LHL_glcm_Idn[1]==input$wavelet.LHL_glcm_Idn[2],1,input$PredN)),
wavelet.LHL_glrlm_HighGrayLevelRunEmphasis=seq(from=input$wavelet.LHL_glrlm_HighGrayLevelRunEmphasis[1],to=input$wavelet.LHL_glrlm_HighGrayLevelRunEmphasis[2],length=ifelse(input$wavelet.LHL_glrlm_HighGrayLevelRunEmphasis[1]==input$wavelet.LHL_glrlm_HighGrayLevelRunEmphasis[2],1,input$PredN)),
wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis=seq(from=input$wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis[1],to=input$wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis[2],length=ifelse(input$wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis[1]==input$wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis[2],1,input$PredN)),
wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis=seq(from=input$wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis[1],to=input$wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis[2],length=ifelse(input$wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis[1]==input$wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis[2],1,input$PredN)),
wavelet.LHL_glszm_HighGrayLevelZoneEmphasis=seq(from=input$wavelet.LHL_glszm_HighGrayLevelZoneEmphasis[1],to=input$wavelet.LHL_glszm_HighGrayLevelZoneEmphasis[2],length=ifelse(input$wavelet.LHL_glszm_HighGrayLevelZoneEmphasis[1]==input$wavelet.LHL_glszm_HighGrayLevelZoneEmphasis[2],1,input$PredN)),
wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis=seq(from=input$wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis[1],to=input$wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis[2],length=ifelse(input$wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis[1]==input$wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis[2],1,input$PredN)),
wavelet.LHL_gldm_HighGrayLevelEmphasis=seq(from=input$wavelet.LHL_gldm_HighGrayLevelEmphasis[1],to=input$wavelet.LHL_gldm_HighGrayLevelEmphasis[2],length=ifelse(input$wavelet.LHL_gldm_HighGrayLevelEmphasis[1]==input$wavelet.LHL_gldm_HighGrayLevelEmphasis[2],1,input$PredN)),
wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis=seq(from=input$wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis[1],to=input$wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis[2],length=ifelse(input$wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis[1]==input$wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis[2],1,input$PredN)),
wavelet.LHH_firstorder_Skewness=seq(from=input$wavelet.LHH_firstorder_Skewness[1],to=input$wavelet.LHH_firstorder_Skewness[2],length=ifelse(input$wavelet.LHH_firstorder_Skewness[1]==input$wavelet.LHH_firstorder_Skewness[2],1,input$PredN)),
wavelet.HLH_glcm_MaximumProbability=seq(from=input$wavelet.HLH_glcm_MaximumProbability[1],to=input$wavelet.HLH_glcm_MaximumProbability[2],length=ifelse(input$wavelet.HLH_glcm_MaximumProbability[1]==input$wavelet.HLH_glcm_MaximumProbability[2],1,input$PredN)),
wavelet.HLH_glszm_GrayLevelNonUniformity=seq(from=input$wavelet.HLH_glszm_GrayLevelNonUniformity[1],to=input$wavelet.HLH_glszm_GrayLevelNonUniformity[2],length=ifelse(input$wavelet.HLH_glszm_GrayLevelNonUniformity[1]==input$wavelet.HLH_glszm_GrayLevelNonUniformity[2],1,input$PredN)),
wavelet.HLH_glszm_SizeZoneNonUniformityNormalized=seq(from=input$wavelet.HLH_glszm_SizeZoneNonUniformityNormalized[1],to=input$wavelet.HLH_glszm_SizeZoneNonUniformityNormalized[2],length=ifelse(input$wavelet.HLH_glszm_SizeZoneNonUniformityNormalized[1]==input$wavelet.HLH_glszm_SizeZoneNonUniformityNormalized[2],1,input$PredN)),
wavelet.HLH_glszm_SmallAreaEmphasis=seq(from=input$wavelet.HLH_glszm_SmallAreaEmphasis[1],to=input$wavelet.HLH_glszm_SmallAreaEmphasis[2],length=ifelse(input$wavelet.HLH_glszm_SmallAreaEmphasis[1]==input$wavelet.HLH_glszm_SmallAreaEmphasis[2],1,input$PredN)),
wavelet.LLL_firstorder_Kurtosis=seq(from=input$wavelet.LLL_firstorder_Kurtosis[1],to=input$wavelet.LLL_firstorder_Kurtosis[2],length=ifelse(input$wavelet.LLL_firstorder_Kurtosis[1]==input$wavelet.LLL_firstorder_Kurtosis[2],1,input$PredN))# Osa 2 alku 
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
 "journal"), 
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
sliderInput(inputId="original_firstorder_Kurtosis",label="original_firstorder_Kurtosis", min =-3.0359906803194,max =2.54385515532817 ,value =c(-3.0359906803194,-3.0359906803194)),
sliderInput(inputId="log.sigma.5.0.mm.3D_glcm_Correlation",label="log.sigma.5.0.mm.3D_glcm_Correlation", min =-2.11572159761415,max =3.21788787996177 ,value =c(-2.11572159761415,-2.11572159761415)),
sliderInput(inputId="wavelet.LLH_firstorder_RootMeanSquared",label="wavelet.LLH_firstorder_RootMeanSquared", min =-3.16239016590501,max =3.29664707345549 ,value =c(-3.16239016590501,-3.16239016590501)),
sliderInput(inputId="wavelet.LLH_glcm_Correlation",label="wavelet.LLH_glcm_Correlation", min =-2.51778663171727,max =3.74876691900733 ,value =c(-2.51778663171727,-2.51778663171727)),
sliderInput(inputId="wavelet.LLH_glcm_Imc2",label="wavelet.LLH_glcm_Imc2", min =-3.24948543013745,max =3.03083426083616 ,value =c(-3.24948543013745,-3.24948543013745)),
sliderInput(inputId="wavelet.LHL_firstorder_Range",label="wavelet.LHL_firstorder_Range", min =-3.1622908597447,max =4.23274079397588 ,value =c(-3.1622908597447,-3.1622908597447)),
sliderInput(inputId="wavelet.LHL_glcm_Autocorrelation",label="wavelet.LHL_glcm_Autocorrelation", min =-2.17827541408756,max =4.38906007638408 ,value =c(-2.17827541408756,-2.17827541408756)),
sliderInput(inputId="wavelet.LHL_glcm_JointAverage",label="wavelet.LHL_glcm_JointAverage", min =-3.25631560715745,max =3.50307785895198 ,value =c(-3.25631560715745,-3.25631560715745)),
sliderInput(inputId="wavelet.LHL_glcm_Idn",label="wavelet.LHL_glcm_Idn", min =-2.3808574461806,max =2.78267034980035 ,value =c(-2.3808574461806,-2.3808574461806)),
sliderInput(inputId="wavelet.LHL_glrlm_HighGrayLevelRunEmphasis",label="wavelet.LHL_glrlm_HighGrayLevelRunEmphasis", min =-2.1906787111354,max =4.36930964875855 ,value =c(-2.1906787111354,-2.1906787111354)),
sliderInput(inputId="wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis",label="wavelet.LHL_glrlm_LongRunHighGrayLevelEmphasis", min =-1.68080765919158,max =4.52083937247815 ,value =c(-1.68080765919158,-1.68080765919158)),
sliderInput(inputId="wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis",label="wavelet.LHL_glrlm_ShortRunHighGrayLevelEmphasis", min =-2.21069119745554,max =4.29653777664388 ,value =c(-2.21069119745554,-2.21069119745554)),
sliderInput(inputId="wavelet.LHL_glszm_HighGrayLevelZoneEmphasis",label="wavelet.LHL_glszm_HighGrayLevelZoneEmphasis", min =-2.13504497706797,max =4.49407110722136 ,value =c(-2.13504497706797,-2.13504497706797)),
sliderInput(inputId="wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis",label="wavelet.LHL_glszm_SmallAreaHighGrayLevelEmphasis", min =-2.11457099618196,max =4.19264410822106 ,value =c(-2.11457099618196,-2.11457099618196)),
sliderInput(inputId="wavelet.LHL_gldm_HighGrayLevelEmphasis",label="wavelet.LHL_gldm_HighGrayLevelEmphasis", min =-2.19649554340391,max =4.36359268664605 ,value =c(-2.19649554340391,-2.19649554340391)),
sliderInput(inputId="wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis",label="wavelet.LHL_gldm_LargeDependenceHighGrayLevelEmphasis", min =-1.43621715429441,max =4.10886468236525 ,value =c(-1.43621715429441,-1.43621715429441)),
sliderInput(inputId="wavelet.LHH_firstorder_Skewness",label="wavelet.LHH_firstorder_Skewness", min =-3.30614816711288,max =2.6355858502449 ,value =c(-3.30614816711288,-3.30614816711288)),
sliderInput(inputId="wavelet.HLH_glcm_MaximumProbability",label="wavelet.HLH_glcm_MaximumProbability", min =-3.25334036219938,max =3.07809651174673 ,value =c(-3.25334036219938,-3.25334036219938)),
sliderInput(inputId="wavelet.HLH_glszm_GrayLevelNonUniformity",label="wavelet.HLH_glszm_GrayLevelNonUniformity", min =-2.77931167618733,max =5.11995503781877 ,value =c(-2.77931167618733,-2.77931167618733)),
sliderInput(inputId="wavelet.HLH_glszm_SizeZoneNonUniformityNormalized",label="wavelet.HLH_glszm_SizeZoneNonUniformityNormalized", min =-2.67965216620746,max =4.69948710627262 ,value =c(-2.67965216620746,-2.67965216620746)),
sliderInput(inputId="wavelet.HLH_glszm_SmallAreaEmphasis",label="wavelet.HLH_glszm_SmallAreaEmphasis", min =-3.05686914622127,max =3.80930942087541 ,value =c(-3.05686914622127,-3.05686914622127)),
sliderInput(inputId="wavelet.LLL_firstorder_Kurtosis",label="wavelet.LLL_firstorder_Kurtosis", min =-3.05815362539087,max =2.44673502913846 ,value =c(-3.05815362539087,-3.05815362539087)),            # Osa 3 alku 
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
