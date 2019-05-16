\name{dLambdadSM5}
\alias{dLambdadSM5}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse Mills Ratio Derivative
}
\description{
Computes a derivative of the inverse Mills ratio with respect to the parameter vector. 
}
\usage{
dLambdadSM5(x, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of exogenous variables
}
  \item{beta}{
vector of parameters
}
}
\details{
This function is necessary for computation of the asymptotic variance. In case of switching regressions the inverse Mills ratio term is different, and its derivative is computed in function \code{dLambdadSM5}. It can also be used to compute the influence function of the two-stage estimator.
}
\value{
The gradient of the inverse Mills ratio is returned as a vector
}
\references{
Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827.
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{ inverse Mills ratio }

