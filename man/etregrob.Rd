\name{etregrob}
\alias{etregrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Fit of Endogenous Treatment Model
}
\description{
Fits the endogenous treatment model using a robust two-stage estimator
}
\usage{
etregrob(selection, outcome, data, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selection}{
formula, the selection equation
}
  \item{outcome}{
formula, the outcome equation
}
  \item{data}{
an optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{etregrob} is called.
}
  \item{control}{
a list of parameters for controlling the fitting process. The same list as for sample selection model
}
}
\details{
Compute robust two-step estimates of the Endogenous Treatment Model. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimator is used instead of traditional OLS. The correction for endogeneity is made by means of control function, which is the inverse Mills ratio for a complete sample (see Maddala, 1983, p. 120-122).
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "etregrob".
  \item{coefficients}{a named vector of coefficients}
  \item{stage1}{object of class \code{glmrob} that contains robust probit fit}
  \item{stage2}{object of class \code{rlm} that contains second stage robust fit. Note that the standard errors in this object are biased, since they are not corrected for the uncertainty in the first estimation step. Use \code{vcov} below}
  \item{vcov}{variance matrix of the second stage}
  \item{sigma}{the standard error of the error term of the outcome equation}
  \item{CIMR}{inverse Mills ratio for the complete sample}
  \item{call}{the matched call}
  \item{method}{method of estimation, currently only "robust two-stage" is implemented}
  \item{converged}{logical. Did all the estimators converge?}
  \item{iterations}{list containing the numbers of iterations}
}
\references{
Maddala G.S. (1983) Limited-Dependent and Qualitative Variables in Econometrics. Cambridge: Cambridge University Press.
}
\author{
Mikhail Zhelonkin
}
\note{
The treatment variable is automatically included in the formula for the second estimation step, i.e. one should not add the dependent variable from the selection equation in the formula of the outcome equation.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[robustbase]{glmrob}}, \code{\link[MASS]{rlm}}, \code{\link{ssmrob}}, \code{\link{heckitrob.control}}
}
\examples{
library(mvtnorm)
set.seed(2)
N <- 3000
beta1 <- c(1.0, 1.0, 0.75)
beta2 <- c(1.5, 1.0, 0.5)
alpha <- 1.25
x1 <- rmvnorm(N, mean = c(0, -1, 1), sigma = diag(c(1, 0.5, 1)))
x2 <- x1 
x2[, 3] <- rnorm(N, 1, 1)
eps <- rmvnorm(N, mean = rep(0, 2), sigma = matrix(c(1, -0.7, -0.7, 1), 2, 2))
x1beta1 <- x1[, 1]*beta1[1] + x1[, 2]*beta1[2] + x1[, 3]*beta1[3]
x2beta2 <- x2[, 1]*beta2[1] + x2[, 2]*beta2[2] + x2[, 3]*beta2[3]
y1 <- ifelse(x1beta1 + eps[, 1] > 0, 1, 0)
y2 <- x2beta2 + alpha*y1 + eps[,2]
etm.ctrl <- heckitrob.control(weights.x1 = "hat", weights.x2 = "covMcd")
etmsim.fit <- etregrob(y1 ~ x1, y2 ~ x2, control = etm.ctrl)
summary(etmsim.fit)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Endogenous treatment }% use one of  RShowDoc("KEYWORDS")
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line
