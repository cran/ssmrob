etregrob <-
function(selection, outcome, control = heckitrob.control())
{
  if (class(outcome) != "formula") {
    stop("argument 'outcome' must be a formula")
  }
  else if (length(selection) != 3) {
    stop("argument 'selection' must be a 2-sided formula")
  }  
  result <- list()
  result$call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("selection", "data", "subset", "weights", "offset"), 
             names(mf), 0)
  mfS <- mf[c(1, m)]
  mfS$na.action <- na.pass
  mfS$drop.unused.levels <- TRUE
  mfS[[1]] <- as.name("model.frame")
  names(mfS)[2] <- "formula"
  mfS <- eval(mfS, parent.frame())
  mtS <- attr(mfS, "terms")
  XS <- model.matrix(mtS, mfS)
  NXS <- ncol(XS)
  YS <- model.response(mfS)
  m <- match(c("outcome", "data", "subset", "weights", "offset"), names(mf), 0)
  mfO <- mf[c(1, m)]
  mfO$na.action <- na.pass
  mfO$drop.unused.levels <- TRUE
  mfO$na.action <- na.pass
  mfO[[1]] <- as.name("model.frame")
  names(mfO)[2] <- "formula"
  mfO <- eval(mfO, parent.frame())
  mtO <- attr(mfO, "terms")
  XO <- model.matrix(mtO, mfO)
  NXO <- ncol(XO)
  YO <- model.response(mfO)
  if(attributes(XS)$dimnames[[2]][1] == "(Intercept)") { XS <- XS[,-1]; seqn <- YS ~ XS} 
  else {seqn <- YS ~ XS - 1}
  result$stage1 <- glmrob(seqn, family = binomial(link = probit),
                          method = "Mqle", weights.on.x = control$weights.x1, 
                          control = glmrobMqle.control(acc = control$acc, 
                          maxit = control$maxit, tcc = control$tcc))
  imrData <- invMillsRatio(result$stage1)
  CIMR <- YS * imrData$IMR1 + (1 - YS) * imrData$IMR0*(-1)
  xMat <- cbind(XO, CIMR)
  xMat1 <- xMat[YS == 1,]
  xMat0 <- xMat[YS == 0,]
  ord <- order(YS, decreasing = T)
  x2weight <- rep(NA, length(YS))
  n <- sum(YS == 1)
  if(control$weights.x2 == "none") x2weight <- rep(1, length(YS)) else
    if(control$weights.x2 == "hat") {
      x2weight1 <- sqrt(1 - hat(xMat1)) 
      x2weight0 <- sqrt(1 - hat(xMat0))
      x2weight[ord[1:n]] <- x2weight1
      x2weight[ord[(n + 1):length(YS)]] <- x2weight0 } else
      if(control$weights.x2 == "robCov")  {
        x2weight1 <- x2weight.robCov(xMat1)
        x2weight0 <- x2weight.robCov(xMat0) 
        x2weight[ord[1:n]] <- x2weight1
        x2weight[ord[(n + 1):length(YS)]] <- x2weight0 } else
        if(control$weights.x2 == "covMcd") {
          x2weight1 <- x2weight.covMcd(xMat1)
          x2weight0 <- x2weight.covMcd(xMat0) 
          x2weight[ord[1:n]] <- x2weight1
          x2weight[ord[(n + 1):length(YS)]] <- x2weight0 }
  result$stage2 <- rlm(YO ~ XO + YS + CIMR - 1, method="M", psi = psi.huber,
                    k=control$t.c, weights = x2weight, maxit = control$maxitO)
  xMat=model.matrix(result$stage2)
  nr.coef <- length(result$stage2$coefficients)
  names(result$stage2$coefficients)[1:(nr.coef-2)] <- substring(names(result$stage2$coefficients)[1:(nr.coef-2)], 3)
  if(names(result$stage1$coefficients)[1]=="(Intercept)")
  { nr.coef <- length(result$stage1$coefficients)
  names(result$stage1$coefficients)[2:nr.coef] <- substring(names(result$stage1$coefficients)[2:nr.coef], 3)
  } else
  { nr.coef <- length(result$stage1$coefficients)
  names(result$stage1$coefficients)[1:nr.coef] <- substring(names(result$stage1$coefficients)[1:nr.coef], 3)
  }
  result$vcov <- etreg2steprobVcov(YS, YO, model.matrix(result$stage1), model.matrix(result$stage2), result$stage1, result$stage2$coeff, result$stage2$s, x2weight, control$t.c)
  result$method <- "robust two-stage"
  class(result) <- c("etregrob", class(result))
  return(result)
}
