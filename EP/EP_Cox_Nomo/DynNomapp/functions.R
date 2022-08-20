getpred.DN <-
function (model, newd, set.rms = F) 
{
    inrange <- T
    mclass <- getclass.DN(model)$model.class
    if (!mclass %in% c("lm", "glm", "coxph", "ols", "lrm", "Glm", 
        "cph", "gam", "Gam", "glmnet")) 
        stop("Unrecognized model object type.")
    if (mclass %in% c("ols", "lrm", "Glm", "cph")) {
        if (set.rms == T) {
            model <- update(model, x = T, y = T, data = data)
        }
        else {
            model <- update(model, x = T, y = T)
        }
    }
    if (mclass %in% c("ols", "lrm", "Glm")) {
        m.pred <- predict(model, newdata = newd, se.fit = TRUE)
        mpred <- m.pred$linear.predictors
        se.pred <- m.pred$se.fit[[1]]
    }
    if (mclass %in% c("glm", "gam")) {
        m.pred <- prediction(model, data = newd, type = "link", 
            calculate_se = TRUE)
        mpred <- m.pred$fit
        se.pred <- m.pred$se.fitted
    }
    if (mclass %in% c("lm", "Gam")) {
        m.pred <- prediction(model, data = newd, calculate_se = TRUE)
        mpred <- m.pred$fit
        se.pred <- m.pred$se.fitted
    }
    if (mclass %in% c("coxph")) {
        if (!any(class(try(prediction(model, data = newd, type = "expected", 
            calculate_se = TRUE), silent = TRUE)) == "try-error")) {
            m.pred <- prediction(model, data = newd, type = "expected", 
                calculate_se = TRUE)
            mpred <- m.pred$fit
            se.pred <- m.pred$se.fit
        }
        else {
            inrange = F
            mpred <- 0
            se.pred <- 0
        }
    }
    if (mclass %in% c("cph")) {
        strata.l <- levels(model$strata)
        if (length(model$strata) != length(levels(attr(predict(model, 
            newd, type = "x", expand.na = FALSE), "strata")))) {
            levels(model$strata) <- levels(attr(predict(model, 
                newd, type = "x", expand.na = FALSE), "strata"))
        }
        m.pred <- suppressWarnings({
            survest(model, newdata = newd, times = newd[, all.vars(model$terms)[1]])
        })
        mpred <- -log(m.pred$surv)
        se.pred <- m.pred$std.err
        if (mpred == 0) {
            inrange = F
        }
    }
    list(pred = mpred, SEpred = se.pred, InRange = inrange)
}
getclass.DN <-
function (model) 
{
    mfamily <- NA
    mclass <- attr(model, "class")[1]
    if (mclass == "coxph.null") 
        stop("Error in model syntax: the model is null")
    if (!mclass %in% c("lm", "glm", "coxph", "ols", "lrm", "Glm", 
        "cph", "gam", "Gam", "glmnet")) 
        stop("Unrecognized model object type.")
    if (mclass %in% c("elnet", "lognet", "multnet", "fishnet", 
        "coxnet", "mrelnet")) {
        mclass <- "glmnet"
        mfamily <- attr(model, "class")[1]
    }
    if (mclass %in% c("glm", "Glm")) 
        mfamily <- model$family$family
    if (mclass == "lrm") 
        mfamily <- mclass
    list(model.class = mclass, model.family = mfamily)
}
