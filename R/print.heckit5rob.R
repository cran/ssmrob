print.heckit5rob <-
function(x, digits = 4, ...)
{
  print(x$method)
  cat("selection stage coefficients: \n")
  print(x$stage1$coeff)
  cat("outcome1 coefficients: \n")
  print(x$stage21$coeff)
  cat("outcome2 coefficients: \n")
  print(x$stage22$coeff)
}
