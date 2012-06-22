\name{ssmrob}
\alias{ssmrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Sample Selection Model
}
\description{
Function provides the robust two-stage estimators of truncated selection model (tobit-2) and switching regression model (tobit-5).
}
\usage{
ssmrob(outcome, selection, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome}{
formula(s), the outcome equation(s)
}
  \item{selection}{
formula, the selection equation
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Outcome equation may be a simple formula for the case of truncated selection model, or a list of two formulas for the case of switching regressions.
}
\value{
Object of class "heckitrob" or object of class "heckit5rob".
}
\references{
Heckman, J.J. (1979). Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2012). Robust Inference in Sample Selection Models, \emph{Manuscript}.

Amemiya, T. (1984). Tobit Models: a Survey. \emph{Journal of Econometrics}, 24, p. 3-61.
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
# sample selection model (tobit-2)
data(MEPS2001)
attach(MEPS2001)
selectEq <- dambexp ~ age+female+educ+blhisp+totchr+ins
outcomeEq <- lnambx ~ age+female+educ+blhisp+totchr+ins
summary(ssmrob(outcomeEq,selectEq,control=heckitrob.control(tcc=3.2,weights.x1="robCov")))


# switching regressions example (tobit-5)
library(mvtnorm)
covm <- diag(3)
covm[lower.tri(covm)] <- c(0.75, 0.5, 0.25)
covm[upper.tri(covm)] <- covm[lower.tri(covm)]
eps <- rmvnorm(1000, rep(0, 3), covm)
x1 <- rnorm(1000)
y1 <- x1 + eps[,1] > 0
x21 <- rnorm(1000)
x22 <- rnorm(1000)
y2=ifelse(y1 > 0.5, x21 + eps[,2], x22 + eps[,3])
summary(ssmrob(list(y2 ~ x21, y2 ~ x22), y1 ~ x1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Robust estimation }
\keyword{ Sample selection }

