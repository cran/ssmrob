print.summary.etregrob <-
function(x, digits = 4, ...)
{
  cat("------------------------------------------------------------- \n")
  cat("Robust Endogenous Treatment Model / 2-step M-estimation \n")
  cat("Probit selection equation: \n")
  names(x$coefficients$selection) <- c("Estimate","Std.Error","t-value","p-value"," ")
  print(x$coefficients$selection, digits = digits)
  cat("Outcome equation: \n")
  names(x$coefficients$outcome) <- c("Estimate","Std.Error","t-value","p-value"," ")
  print(x$coefficients$outcome, digits = digits)
  cat("---\n")
  cat("Signif. codes: 0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1 \n")
  cat("-------------------------------------------------------------")
}
