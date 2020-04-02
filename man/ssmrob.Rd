\name{ssmrob}
\alias{ssmrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Sample Selection Model
}
\description{
Compute robust two-stage estimates of truncated selection model (Tobit-2) and switching regression model (Tobit-5).
}
\usage{
ssmrob(selection, outcome, data, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selection}{
formula, the selection equation
}
  \item{outcome}{
formula(s), the outcome equation(s)
}
  \item{data}{
an optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{ssmrob} is called.
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Outcome equation may be a simple formula for the case of Heckman selection model, or a list of two formulas for the case of switching regressions.
}
\value{
Object of class "heckitrob" or object of class "heckit5rob".
}
\references{
Amemiya, T. (1984) Tobit Models: a Survey. \emph{Journal of Econometrics}, 24, p. 3-61.

Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827. \doi{10.1111/rssb.12136}
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{heckitrob}}, \code{\link{heckit5rob}}
}
\examples{
# sample selection model (Tobit-2)
# Zhelonkin, Genton, Ronchetti (2016): page 823
data(MEPS2001)
selectEq <- dambexp ~ age + female + educ + blhisp + totchr + ins
outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
meps.fit <- ssmrob(selectEq, outcomeEq, data = MEPS2001, control = heckitrob.control(tcc = 3.2))
summary(meps.fit)

# switching regressions example (Tobit-5)
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
\keyword{ Robust estimation }
\keyword{ Sample selection }

