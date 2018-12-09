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
heckit5rob(outcome1, outcome2, selection, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome1}{
formula, first outcome equation
}
  \item{outcome2}{
formula, second outcome equation
}
  \item{selection}{
formula, the selection equation
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Function provides a robust two-stage extimator of the switching regression model with probit selection. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimators are used.
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "heckit5rob".
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827. 
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
summary(heckit5rob(y2 ~ x21, y2 ~ x22, y1 ~ x1))}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample selection }
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line

