coef.etregrob <-
function(object, ...)
{
  coeff <- list()
  coeff$S <- coef(object$stage1)
  coeff$O <- coef(object$stage2)
  class(coeff) <- c("coef.etregrob", class(coeff), class(coeff$S))
  return(coeff)
}
