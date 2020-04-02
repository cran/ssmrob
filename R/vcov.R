vcov.heckitrob <- function(object, ...)
{
  return(object$vcov)
}
vcov.heckit5rob <- function(object, ...)
{
  ret=list()
  ret$regime1=object$vcov1
  ret$regime2=object$vcov2
  return(ret)
}
vcov.etregrob <- function(object, ...)
{
  return(object$vcov)
}

