\name{heckit5rob}
\alias{heckit5rob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Heckit Fit: Switching Regressions
}
\description{
Fits the switching regression model with probit selection using a robust two-stage estimator
}
\usage{
heckit5rob(selection, outcome1, outcome2, data, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selection}{
formula, the selection equation
}
  \item{outcome1}{
formula, first outcome equation
}
  \item{outcome2}{
formula, second outcome equation
}
  \item{data}{
an optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{heckit5rob} is called.
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Compute robust two-stage extimates of the switching regression model with probit selection. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimators are used.
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "heckit5rob".
%%  If it is a LIST, use
  \item{coefficients}{a named vector of coefficients}
  \item{stage1}{object of class \code{glmrob} that contains (first stage) robust probit fit}
  \item{stage21}{object of class \code{rlm} that contains second stage first regime robust fit. Note that the standard errors in this object are biased, since they are not corrected for the uncertainty in the first estimation step. Use \code{vcov1} below}
  \item{stage22}{object of class \code{rlm} that contains second stage second regime robust fit. Note that the standard errors in this object are biased, since they are not corrected for the uncertainty in the first estimation step. Use \code{vcov2} below}  
  \item{vcov1}{variance matrix of the second stage first regime}
  \item{vcov2}{variance matrix of the second stage second regime}
  \item{sigma1}{the standard error of the error term of the outcome equation when y_1=1}
  \item{sigma2}{the standard error of the error term of the outcome equation when y_1=0}
  \item{IMR1}{inverse Mills ratio for the case when y_1=1}
  \item{IMR2}{inverse Mills ratio for the case when y_1=0}
  \item{call}{the matched call}
  \item{method}{method of estimation, currently only "robust two-stage" is implemented}
  \item{converged}{logical. Did all the estimators converge?}
  \item{iterations}{list containing the numbers of iterations}
}
\references{
Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827. \doi{10.1111/rssb.12136}

Zhelonkin, M., Ronchetti, E. (2021) Robust Analysis of Sample Selection Models through the R Package ssmrob. \emph{Journal of Statistical Software}, 99, 4, p. 1-35. \doi{10.18637/jss.v099.i04}
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[robustbase]{glmrob}}, \code{\link[MASS]{rlm}}, \code{\link{ssmrob}}, \code{\link{heckitrob}}, \code{\link{heckitrob.control}}
}
\examples{
\dontrun{
library(mvtnorm)
set.seed(2)
N <- 5000
beta1 <- c(0, 1.0, 1.0, 0.75)
beta21 <- c(0, 1.5, 1.0, 0.5)
beta22 <- c(1, -1.5, 1.0, 0.5)
covm <- diag(3)
covm[lower.tri(covm)] <- c(0.75, 0.5, 0.25)
covm[upper.tri(covm)] <- covm[lower.tri(covm)]
eps <- rmvnorm(N, rep(0, 3), covm)
x1 <- rmvnorm(N, mean=c(0, -1, 1), sigma=diag(c(1, 0.5, 1)))
x21 <- x1 
x22 <- x1
x21[, 3] <- rnorm(N, 1, 1)
x22[, 3] <- rnorm(N, 1, 1)
x1beta1 <- beta1[1] + x1[, 1]*beta1[2] + x1[, 2]*beta1[3] + x1[, 3]*beta1[4]
x21beta21 <- beta21[1] + x21[, 1]*beta21[2] + x21[, 2]*beta21[3] + x21[, 3]*beta21[4]
x22beta22 <- beta22[1] + x22[, 1]*beta22[2] + x22[, 2]*beta22[3] + x22[, 3]*beta22[4]
y1 <- ifelse(x1beta1 + eps[, 1] > 0, 1, 0)
y2 <- ifelse(y1 > 0.5, x21beta21 + eps[, 2],
x22beta22 + eps[, 3])
srsim.fit <- ssmrob(y1 ~ x1, list(y2 ~ x21, y2 ~ x22), 
control = heckitrob.control(weights.x1 = "hat", weights.x2 = "covMcd"))
summary(srsim.fit)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample selection }
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line

