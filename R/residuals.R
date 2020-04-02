residuals.heckitrob <- function(object, ...)
{
  return(resid(object$stage2))
}
residuals.heckit5rob <- function(object, ...)
{
  res <- list()
  res$regime1 <- resid(object$stage21)
  res$regime2 <- resid(object$stage22)
  return(res)
}
residuals.etregrob <- function(object, ...)
{
  return(resid(object$stage2))
}
