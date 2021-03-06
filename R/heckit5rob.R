heckit5rob <-
function(selection, outcome1, outcome2, data, control=heckitrob.control())
{
  if (class(outcome1) != "formula") {
    stop("argument 'outcome1' must be a formula")
  } 
  else  if (class(outcome2) != "formula") {
    stop("argument 'outcome2' must be a formula")
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
    m <- match(c("outcome1", "data", "subset", "weights", "offset"), 
               names(mf), 0)
    mfO1 <- mf[c(1, m)]
    mfO1$na.action <- na.pass
    mfO1$drop.unused.levels <- TRUE
    mfO1$na.action <- na.pass
    mfO1[[1]] <- as.name("model.frame")
    names(mfO1)[2] <- "formula"
    mfO1 <- eval(mfO1, parent.frame())
    mtO1 <- attr(mfO1, "terms")
    XO1 <- model.matrix(mtO1, mfO1)
    NXO1 <- ncol(XO1)
    YO1 <- model.response(mfO1)
  m <- match(c("outcome2", "data", "subset", "weights", "offset"), 
             names(mf), 0)
  mfO2 <- mf[c(1, m)]
  mfO2$na.action <- na.pass
  mfO2$drop.unused.levels <- TRUE
  mfO2$na.action <- na.pass
  mfO2[[1]] <- as.name("model.frame")
  names(mfO2)[2] <- "formula"
  mfO2 <- eval(mfO2, parent.frame())
  mtO2 <- attr(mfO2, "terms")
  XO2 <- model.matrix(mtO2, mfO2)
  NXO2 <- ncol(XO2)
  YO2 <- model.response(mfO2)
  if(attributes(XS)$dimnames[[2]][1] == "(Intercept)") { XS <- XS[,-1]; seqn <- YS ~ XS} 
  else {seqn <- YS ~ XS - 1}
  result$stage1 <- glmrob(seqn, family = binomial(link = probit),
                          method="Mqle", weights.on.x = control$weights.x1, 
                          control = glmrobMqle.control(acc = control$acc, 
                                    maxit=control$maxit, tcc = control$tcc))
  imrData <- invMillsRatio(result$stage1)
  IMR1 <- imrData$IMR1
  IMR2 <- -imrData$IMR0
  xMat1 <- cbind(XO1, IMR1)
  xMat2 <- cbind(XO2, IMR2)
  if(control$weights.x2 == "none") x2weight1 <- rep(1, length(YS)) else
    if(control$weights.x2 == "hat") x2weight1 <- sqrt(1 - hat(xMat1)) else
      if(control$weights.x2 == "robCov") x2weight1 <- x2weight.robCov(xMat1) else
        if(control$weights.x2 == "covMcd") x2weight1 <- x2weight.covMcd(xMat1)
  if(control$weights.x2 == "none") x2weight2 <- rep(1, length(YS)) else
    if(control$weights.x2 == "hat") x2weight2 <- sqrt(1-hat(xMat2)) else
      if(control$weights.x2 == "robCov") x2weight2 <- x2weight.robCov(xMat2) else
        if(control$weights.x2 == "covMcd") x2weight2 <- x2weight.covMcd(xMat2)
  result$stage21 <- rlm(YO1 ~ XO1 + IMR1 - 1, method="M", psi = psi.huber, k=control$t.c, weights=x2weight1, maxit=control$maxitO, subset=YS==1)
  result$stage22 <- rlm(YO2 ~ XO2 + IMR2 - 1, method="M", psi = psi.huber, k=control$t.c, weights=x2weight2, maxit=control$maxitO, subset=YS==0)
  result$IMR1 <- IMR1
  result$IMR2 <- IMR2
  xMat1 <- model.matrix(result$stage21)
  xMat2 <- model.matrix(result$stage22)
  x2weight1 <- subset(x2weight1, YS==1)
  x2weight2 <- subset(x2weight2, YS==0)
  result$vcov1 <- heck2steprobVcov(YS[YS==1], YO1[YS==1], model.matrix(result$stage1)[YS==1,], xMat1, result$stage1, result$stage21$coeff, result$stage21$s, x2weight1, control$t.c)
  result$vcov2 <- heck5twosteprobVcov(YS[YS==0], YO2[YS==0], model.matrix(result$stage1)[YS==0,], xMat2, result$stage1, result$stage22$coeff, result$stage22$s, x2weight2, control$t.c)
  nr.coef <- length(result$stage21$coefficients)
  names(result$stage21$coefficients)[1:(nr.coef-1)] <- substring(names(result$stage21$coefficients)[1:(nr.coef-1)], 4)
  names(result$stage21$coefficients)[nr.coef] <- substring(names(result$stage21$coefficients)[nr.coef], 1)
  nr.coef <- length(result$stage22$coefficients)
  names(result$stage22$coefficients)[1:(nr.coef-1)] <- substring(names(result$stage22$coefficients)[1:(nr.coef-1)], 4)
  names(result$stage22$coefficients)[nr.coef] <- substring(names(result$stage22$coefficients)[nr.coef], 1)
  if(names(result$stage1$coefficients)[1]=="(Intercept)")
  { nr.coef <- length(result$stage1$coefficients)
  names(result$stage1$coefficients)[2:nr.coef] <- substring(names(result$stage1$coefficients)[2:nr.coef], 3)
  } else
  { nr.coef <- length(result$stage1$coefficients)
  names(result$stage1$coefficients)[1:nr.coef] <- substring(names(result$stage1$coefficients)[1:nr.coef], 3)
  }
  result$coefficients <- c(result$stage1$coefficients, result$stage21$coefficients, result$stage22$coefficients)
  result$sigma1 <- drop(sqrt(crossprod(result$stage21$residuals)/sum(YS) + mean(drop(imrData$delta1)[YS == 1]) * result$stage21$coefficients["IMR1"]^2))
  result$sigma2 <- drop(sqrt(crossprod(result$stage22$residuals)/sum(YS==0) + mean(drop(imrData$delta0)[YS == 0]) * result$stage22$coefficients["IMR2"]^2))
  result$converged <- result$stage1$converged & result$stage21$converged & result$stage22$converged
  result$iterations <- list(iter1 = result$stage1$iter, iter21 = length(result$stage21$conv), iter22 = length(result$stage22$conv))
  result$method <- "robust two-stage"
  class(result) <- c("heckit5rob", class(result))
  return(result)
}
