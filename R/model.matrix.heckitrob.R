model.matrix.heckitrob <-
function(object, part = "outcome", ...)
{
  if(part == "outcome")
  return(model.matrix(object$stage2))
  else if(part == "selection")
    return(model.matrix(object$stage1))
  else cat("Please choose either 'outcome' or 'selection' matrix")
}
