server = function(input, output){
observe({if (input$quit == 1)
          stopApp()})

limits <- reactive({ if (input$limits) { limits <- c(input$lxlim, input$uxlim) } else {
                         limits <- limits0 } })

output$manySliders <- renderUI({
  slide.bars <- list()
               for (j in 1:length(preds)){
               if (terms[j+1] == "factor"){
               slide.bars[[j]] <- list(selectInput(paste("pred", j, sep = ""), names(preds)[j], preds[[j]]$v.levels, multiple = FALSE))
               }
               if (terms[j+1] == "numeric"){
               if (covariate == "slider") {
               slide.bars[[j]] <- list(sliderInput(paste("pred", j, sep = ""), names(preds)[j],
               min = preds[[j]]$v.min, max = preds[[j]]$v.max, value = preds[[j]]$v.mean))
               }
               if (covariate == "numeric") {
               slide.bars[[j]] <- list(numericInput(paste("pred", j, sep = ""), names(preds)[j], value = zapsmall(preds[[j]]$v.mean, digits = 4)))
               }}}
               do.call(tagList, slide.bars)
})

output$setlimits <- renderUI({
        if (is.null(DNlimits)){
               setlim <- list(checkboxInput("limits", "Set x-axis ranges"),
               conditionalPanel(condition = "input.limits == true",
               numericInput("uxlim", "x-axis upper", zapsmall(limits0[2], digits = 2)),
               numericInput("lxlim", "x-axis lower", zapsmall(limits0[1], digits = 2))))
        } else{ setlim <- NULL }
        setlim
})

a <- 0
new.d <- reactive({
               input$add
               input.v <- vector("list", length(preds))
               for (i in 1:length(preds)) {
               input.v[[i]] <- isolate({
               input[[paste("pred", i, sep = "")]]
               })
               names(input.v)[i] <- names(preds)[i]
               }
               out <- data.frame(lapply(input.v, cbind))
               if (a == 0) {
               input.data <<- rbind(input.data, out)
               }
               if (a > 0) {
               if (!isTRUE(compare(old.d, out))) {
               input.data <<- rbind(input.data, out)
               }}
               a <<- a + 1
               out
})

p1 <- NULL
old.d <- NULL
data2 <- reactive({
               if (input$add == 0)
               return(NULL)
               if (input$add > 0) {
               if (!isTRUE(compare(old.d, new.d()))) {
               isolate({
               mpred <- getpred.DN(model, new.d(), set.rms=T)$pred
               se.pred <- getpred.DN(model, new.d(), set.rms=T)$SEpred
               if (is.na(se.pred)) {
               lwb <- "No standard errors"
               upb <- "by 'glm'"
               pred <- mlinkF(mpred)
               d.p <- data.frame(Prediction = zapsmall(pred, digits = 3),
               Lower.bound = lwb, Upper.bound = upb)
               } else {
               lwb <- sort(mlinkF(mpred + cbind(1, -1) * (qnorm(1 - (1 - clevel)/2) * se.pred)))[1]
               upb <- sort(mlinkF(mpred + cbind(1, -1) * (qnorm(1 - (1 - clevel)/2) * se.pred)))[2]
               pred <- mlinkF(mpred)
               d.p <- data.frame(Prediction = zapsmall(pred, digits = 3),
               Lower.bound = zapsmall(lwb, digits = 3),
               Upper.bound = zapsmall(upb, digits = 3))
               }
               old.d <<- new.d()
               data.p <- cbind(d.p, counter = 1, count=0)
               p1 <<- rbind(p1, data.p)
               p1$counter <- seq(1, dim(p1)[1])
               p1$count <- 0:(dim(p1)[1]-1) %% 11 + 1
               p1
               })
               } else {
               p1$count <- seq(1, dim(p1)[1])
               }}
               rownames(p1) <- c()
               p1
})

output$plot <- renderPlotly({
  if (input$add == 0)
               return(NULL)
               if (is.null(new.d()))
               return(NULL)
               coll=c("#0E0000", "#0066CC", "#E41A1C", "#54A552", "#FF8000", "#BA55D3",
               "#006400", "#994C00", "#F781BF", "#00BFFF", "#A9A9A9")
               lim <- limits()
               yli <- c(0 - 0.5, 10 + 0.5)
               dat2 <- data2()
               if (dim(data2())[1] > 11){
               input.data = input.data[-c(1:(dim(input.data)[1]-11)),]
               dat2 <- data2()[-c(1:(dim(data2())[1]-11)),]
               yli <- c(dim(data2())[1] - 11.5, dim(data2())[1] - 0.5)
               }
               in.d <- input.data
               xx <- matrix(paste(names(in.d), ": ", t(in.d), sep = ""), ncol = dim(in.d)[1])
               Covariates <- apply(xx, 2, paste, collapse = "<br />")
               p <- ggplot(data = dat2, aes(x = Prediction, y = counter - 1, text = Covariates,
               label = Prediction, label2 = Lower.bound, label3=Upper.bound)) +
               geom_point(size = 2, colour = coll[dat2$count], shape = 15) +
               ylim(yli[1], yli[2]) + coord_cartesian(xlim = lim) +
               labs(title = "95% Confidence Interval for Response",
               x = "Probability", y = "") + theme_bw() +
               theme(axis.text.y = element_blank(), text = element_text(face = "bold", size = 10))
               if (is.numeric(dat2$Upper.bound)){
               p <- p + geom_errorbarh(xmax = dat2$Upper.bound, xmin = dat2$Lower.bound,
               size = 1.45, height = 0.4, colour = coll[dat2$count])
               } else{
               message("Confidence interval is not available as there is no standard errors available by 'glm' ")
               }
               gp <- ggplotly(p, tooltip = c("text", "label", "label2", "label3"))
               gp$elementId <- NULL
               gp
})

output$data.pred <- renderPrint({
  if (input$add > 0) {
               if (nrow(data2()) > 0) {
               if (dim(input.data)[2] == 1) {
               in.d <- data.frame(input.data)
               names(in.d) <- names(terms)[2]
               data.p <- cbind(in.d, data2()[1:3])
               }
               if (dim(input.data)[2] > 1) {
               data.p <- cbind(input.data, data2()[1:3])
               }}
               stargazer(data.p, summary = FALSE, type = "text")
}
})

output$summary <- renderPrint({
summary(model)
})
}
