\name{x2weight.robCov}
\alias{x2weight.robCov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robustness weights computation
}
\description{
Auxiliary function. Computation of the leverage weights based on robust Mahalanobis distance. For computation of location and scatter the MVE method is used.
}
\usage{
x2weight.robCov(xMat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xMat}{
matrix of explanatory variables
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
vector of weights
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti

}
%\note{
%%  ~~further notes~~
%}


\seealso{
\code{\link[MASS]{cov.rob}}
}
\keyword{ Minimum volume ellipsoid }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

