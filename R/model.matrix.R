model.matrix.heckitrob <-
function(object, part = "outcome", ...)
{
  if(part == "outcome")
  return(model.matrix(object$stage2))
  else if(part == "selection")
    return(model.matrix(object$stage1))
  else cat("Please choose either 'outcome' or 'selection' matrix")
}
model.matrix.heckit5rob <- function(object, part = "outcome", ...)
{
  if(part == "outcome")
    return(list(stage21 = model.matrix(object$stage21), stage22 = model.matrix(object$stage22)))
  else if(part == "selection")
    return(model.matrix(object$stage1))
  else cat("Please choose either 'outcome' or 'selection' matrix")
}
model.matrix.etregrob <- function(object, part = "outcome", ...)
{
  if(part == "outcome")
    return(model.matrix(object$stage2))
  else if(part == "selection")
    return(model.matrix(object$stage1))
  else cat("Please choose either 'outcome' or 'selection' matrix")
}
