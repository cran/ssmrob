\name{heckitrob}
\alias{heckitrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Heckit Fit
}
\description{
Fits the sample selection model using a robust two-stage estimator
}
\usage{
heckitrob(outcome, selection, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome}{
formula, the outcome equation
}
  \item{selection}{
formula, the selection equation
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Function provides a robust two-stage extimator of the Heckman's selection model. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimator is used. 
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "heckitrob".
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2013) Robust Inference in Sample Selection Models. \emph{Manuscript}.
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[robustbase]{glmrob}}, \code{\link[MASS]{rlm}}, \code{\link{ssmrob}}, \code{\link{heckitrob.control}}, \code{\link{heckit5rob}}
}
\examples{
data(MEPS2001)
attach(MEPS2001)
selectEq <- dambexp ~ age+female+educ+blhisp+totchr+ins
outcomeEq <- lnambx ~ age+female+educ+blhisp+totchr+ins
summary(heckitrob(outcomeEq,selectEq,control=heckitrob.control(tcc=3.2,weights.x1="robCov")))
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample selection }
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line
