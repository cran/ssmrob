print.heckitrob <-
function(x, digits = 4, ...)
{
  cat("Call: \n")
  print(x$call)
  cat(" \n")
  cat("Coefficients (selection equation): \n")
  print(x$stage1$coeff, digits = 4)
  cat("Coefficients (outcome equation): \n")
  print(x$stage2$coeff, digits = 4)
}
