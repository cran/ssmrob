coef.heckitrob <- function(object, ...)
{
  coeff <- list()
  coeff$S <- coef(object$stage1)
  coeff$O <- coef(object$stage2)
  class(coeff) <- c("coef.heckitrob", class(coeff), class(coeff$S))
  return(coeff)
}
coef.heckit5rob <- function(object, ...)
{
  coeff <- list()
  coeff$S <- coef(object$stage1)
  coeff$O1 <- coef(object$stage21)
  coeff$O2 <- coef(object$stage22)
  class(coeff) <- c("coef.heckit5rob", class(coeff), class(coeff$S))
  return(coeff)
}
coef.etregrob <- function(object, ...)
{
  coeff <- list()
  coeff$S <- coef(object$stage1)
  coeff$O <- coef(object$stage2)
  class(coeff) <- c("coef.etregrob", class(coeff), class(coeff$S))
  return(coeff)
}
