\name{PsiMest}
\alias{PsiMest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Score Function of the Mallows M-Estimator
}
\description{
Score function of the Mallows-type M-estimator. 
}
\usage{
PsiMest(x, y, beta, sigma, t.c, weight)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of exogenous variables
}
  \item{y}{
endogenous variable
}
  \item{beta}{
parameter vector
}
  \item{sigma}{
std.error
}
  \item{t.c}{
tuning constant of Huber psi function
}
  \item{weight}{
vector of weights on the exogenous variables
}
}
\details{
Can be used to compute the influence function of the estimator. Also can be used to approximate the bias of the estimator.
}
\value{
returns the vector of scores
}
\references{
Hampel, Ronchetti, Rousseeuw, Stahel (1986)
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Psi-function }
\keyword{ Influence Function }  % __ONLY ONE__ keyword per line
