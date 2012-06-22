\name{x2weight.covMcd}
\alias{x2weight.covMcd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robustness weights computation
}
\description{
Auxiliary function. Computation of the leverage weights based on robust Mahalanobis distance. For computation of location and scatter the MCD method is used.
}
\usage{
x2weight.covMcd(xMat)
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
\keyword{ Minimum covariance determinant }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
