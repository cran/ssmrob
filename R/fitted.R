fitted.etregrob <- function(object, ...)
{
  return(fitted(object$stage2))
}
fitted.heckit5rob <- function(object, ...)
{
  res=list()
  res$regime1 <- fitted(object$stage21)
  res$regime2 <- fitted(object$stage22)
  return(res)
}
fitted.heckitrob <- function(object, ...)
{
  return(fitted(object$stage2))
}
