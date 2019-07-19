summary.etregrob <-
function(object, ...)
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
