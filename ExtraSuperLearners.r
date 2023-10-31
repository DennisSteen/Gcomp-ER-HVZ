SL.gam2 <- function(...) {
  SL.gam(...,cts.num = 8)
}

SL.glmnet.ridge <- function(...){
  SL.glmnet2(..., alpha = 0)
}

SL.stepGAM <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 8, direction = "forward",
                        ...) 
{
  if ("mgcv" %in% loadedNamespaces()) 
    warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+"), 
                                  "+", paste(colnames(X[, !cts.x, drop = FALSE]), 
                                             collapse = "+")))
  } else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(", 
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam, 
                                                    ")", sep = ""), collapse = "+")))
  }
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X), 
                                              collapse = "+"), sep = ""))
  }
  full.fit.gam <- gam::gam(Y ~ 1, data = X, family = family, 
                           control = gam::gam.control(maxit = 50, bf.maxit = 50), 
                           weights = obsWeights)
  
  
  scope.vector <- c(lapply(names(cts.x[cts.x]),function(x){c("1",x,paste0("s(",x,",",deg.gam,")")) }),lapply(names(cts.x[!cts.x]),function(x){c("1",x) }))
  
  fit.gam <- step.Gam(full.fit.gam, scope = scope.vector, direction = direction, data = X)
  
  if (packageVersion("gam") >= "1.15") {
    pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
  }
  else {
    stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}

SL.randomForest2 <- function (Y, X, newX, family, mtry = ifelse(family$family == 
                                              "gaussian", max(floor(ncol(X)/3), 1), floor(sqrt(ncol(X)))), 
          ntree = 1000, nodesize = ifelse(family$family == "gaussian", 
                                          5, 1), maxnodes = NULL, importance = FALSE, ...) 
{
  if (family$family == "gaussian") {
    fit.rf <- randomForest::randomForest(Y ~ ., data = X, 
                                         ntree = ntree, xtest = newX, keep.forest = TRUE, 
                                         mtry = mtry, nodesize = nodesize, maxnodes = maxnodes, 
                                         importance = importance)
    pred <- fit.rf$test$predicted
    fit <- list(object = fit.rf)
  }
  if (family$family == "binomial") {
    fit.rf <- randomForest::randomForest(y = Y, 
                                         x = X, ntree = ntree, xtest = newX, keep.forest = TRUE, 
                                         mtry = mtry, nodesize = nodesize, maxnodes = maxnodes, 
                                         importance = importance)
    pred <- fit.rf$test$predicted
    fit <- list(object = fit.rf)
  }
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.randomForest")
  return(out)
}

SL.ranger2 <- function (Y, X, newX, family, obsWeights, num.trees = 500, mtry = floor(sqrt(ncol(X))), 
          write.forest = TRUE, probability = family$family == "binomial", 
          min.node.size = 5, replace = TRUE, sample.fraction = ifelse(replace, 
                                                                                 1, 0.632), num.threads = 1, verbose = T, ...) 
{
  if (is.matrix(X)) {
    X = data.frame(X)
  }
  fit <- ranger::ranger(`_Y` ~ ., data = cbind(`_Y` = Y, 
                                               X), num.trees = num.trees, mtry = mtry, min.node.size = min.node.size, 
                        replace = replace, sample.fraction = sample.fraction, 
                        case.weights = obsWeights, write.forest = write.forest, 
                        probability = probability, num.threads = num.threads, 
                        verbose = verbose)
  pred <- predict(fit, data = newX)$predictions
  if (family$family == "binomial") {
    pred = pred[, "1"]
  }
  fit <- list(object = fit, verbose = verbose)
  class(fit) <- c("SL.ranger")
  out <- list(pred = pred, fit = fit)
  return(out)
}

SL.glmnet2 <- function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = 10, 
          nlambda = 100, useMin = TRUE, loss = "deviance", ...) 
{
  if (!is.matrix(X)) {
    X <- model.matrix(~-1 + ., X)
    newX <- model.matrix(~-1 + ., newX)
  }
  if(family$family == "binomial"){
    Yc <- cbind(Y,rep(1,length(Y)))
  }
  fitCV <- glmnet::cv.glmnet(x = X, y = Yc, weights = obsWeights, 
                             lambda = NULL, type.measure = loss, nfolds = nfolds, 
                             family = family$family, alpha = alpha, nlambda = nlambda, 
                             ...)
  pred <- predict(fitCV, newx = newX, type = "response", 
                  s = ifelse(useMin, "lambda.min", "lambda.1se"))
  fit <- list(object = fitCV, useMin = useMin)
  class(fit) <- "SL.glmnet"
  out <- list(pred = pred, fit = fit)
  return(out)
}