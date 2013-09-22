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
This function is necessary for computation of the asymptotic variance. In case of switching regressions the inverse Mills ratio term is different, and its derivative is computed in function \code{dLambdadSM5}. It can be also used to compute the influence function of the two-stage estimator.
}
\value{
returns the gradient of the inverse Mills ratio
}
\references{
Zhelonkin, Genton and Ronchetti (2012)
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{ inverse Mills ratio }

