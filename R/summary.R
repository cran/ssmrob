summary.heckitrob <- function(object, ...)
{
  seS <- sqrt(diag(vcov(object$stage1)))
  seO <- sqrt(diag(vcov(object)))
  tvalS <- coef(object)$S / seS
  tvalO <- coef(object)$O / seO
  pvalS <- 2 * pnorm(-abs(coef(object)$S / seS))
  pvalO <- 2 * pnorm(-abs(coef(object)$O / seO))
  SignifS <- symnum(pvalS, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                   symbols = c("***", "** ", "*  ", ".  ", " "))
  SignifO <- symnum(pvalO, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  nms = c("Estimate","Std. Error","t value","Pr(>|t|)", "")
  TAB <- list(selection=data.frame(Estimate=coef(object)$S, Std.Err=seS, t.value=signif(tvalS, digits=4), p.value=signif(pvalS, digits=3), Sign= as.vector(SignifS) ),
           outcome=data.frame(Estimate=coef(object)$O, Std.Err=seO, t.value=signif(tvalO, digits=4), p.value=signif(pvalO, digits=3), Sign= as.vector(SignifO)) )
  colnames(TAB$selection) <- nms
  colnames(TAB$outcome) <- nms
  res <- list(call = object$call, coefficients = TAB)
  res$nobs <- length(object$stage1$fitted)
  res$nobs2 <- length(object$stage2$fitted)
  res$sigma <- object$sigma
  class(res) <- "summary.heckitrob"
  return(res)
}
summary.heckit5rob <- function(object, ...)
{
  seS <- sqrt(diag(vcov(object$stage1)))
  seO1 <- sqrt(diag(vcov(object)$regime1))
  seO2 <- sqrt(diag(vcov(object)$regime2))
  tvalS <- coef(object)$S/seS
  tvalO1 <- coef(object)$O1/seO1
  tvalO2 <- coef(object)$O2/seO2
  pvalS <- 2 * pnorm(-abs(coef(object)$S/seS))
  pvalO1 <- 2 * pnorm(-abs(coef(object)$O1/seO1))
  pvalO2 <- 2 * pnorm(-abs(coef(object)$O2/seO2))
  SignifS <- symnum(pvalS, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  SignifO1 <- symnum(pvalO1, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "** ", "*  ", ".  ", " "))
  SignifO2 <- symnum(pvalO2, corr = FALSE, na = FALSE, 
                     cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                     symbols = c("***", "** ", "*  ", ".  ", " "))
  nms = c("Estimate","Std. Error","t value","Pr(>|t|)", "")
  TAB <- list(selection=data.frame(Estimate=coef(object)$S, Std.Err=seS, t.value=signif(tvalS, digits=4), p.value=signif(pvalS, digits=3), Signif=as.vector(SignifS) ),
              outcome1=data.frame(Estimate=coef(object)$O1, Std.Err=seO1, t.value=signif(tvalO1, digits=4), p.value=signif(pvalO1, digits=3), Signif=as.vector(SignifO1)),
              outcome2=data.frame(Estimate=coef(object)$O2, Std.Err=seO2, t.value=signif(tvalO2, digits=4), p.value=signif(pvalO2, digits=3), Signif=as.vector(SignifO2)))
  colnames(TAB$selection) <- nms
  colnames(TAB$outcome1) <- nms
  colnames(TAB$outcome2) <- nms
  res <- list(call=object$call,coefficients=TAB)
  res$nobs <- length(object$stage1$fitted)
  res$nobs1 <- length(object$stage21$fitted)
  res$nobs2 <- length(object$stage22$fitted)
  res$sigma1 <- object$sigma1
  res$sigma2 <- object$sigma2
  class(res) <- "summary.heckit5rob"
  return(res)
}
summary.etregrob <- function(object, ...)
{
  seS <- sqrt(diag(vcov(object$stage1)))
  seO <- sqrt(diag(vcov(object)))
  tvalS <- coef(object)$S / seS
  tvalO <- coef(object)$O / seO
  pvalS <- 2 * pnorm(-abs(coef(object)$S / seS))
  pvalO <- 2 * pnorm(-abs(coef(object)$O / seO))
  SignifS <- symnum(pvalS, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  SignifO <- symnum(pvalO, corr = FALSE, na = FALSE, 
                    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                    symbols = c("***", "** ", "*  ", ".  ", " "))
  nms = c("Estimate","Std. Error","t value","Pr(>|t|)", "")
  TAB <- list(selection=data.frame(Estimate=coef(object)$S, Std.Err=seS, t.value=signif(tvalS, digits=4), p.value=signif(pvalS, digits=3), Sign= as.vector(SignifS) ),
              outcome=data.frame(Estimate=coef(object)$O, Std.Err=seO, t.value=signif(tvalO, digits=4), p.value=signif(pvalO, digits=3), Sign= as.vector(SignifO)) )
  colnames(TAB$selection) <- nms
  colnames(TAB$outcome) <- nms
  res <- list(call = object$call, coefficients = TAB)
  res$nobs <- length(object$stage1$fitted)
  res$sigma <- object$sigma
  class(res) <- "summary.etregrob"
  return(res)
}
