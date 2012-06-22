\name{dLambdadSM}
\alias{dLambdadSM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse Mills Ratio Derivative
}
\description{
Computes a derivative of the inverse Mills ratio with respect to the parameter vector. 
}
\usage{
dLambdadSM(x, beta)
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
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Zhelonkin, M., Genton M.G., and Ronchetti, E. (2012) Robust Inference in Sample Selection Models. \emph{Manuscript}.
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (x, beta) 
%{
%    (-pnorm(t(x) \%*\% beta) * dnorm(t(x) \%*\% beta) * (t(x) \%*\% 
%        beta) - dnorm(t(x) \%*\% beta)^2)/pnorm(t(x) \%*\% beta)/pnorm(t(x) \%*\% 
%        beta)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ inverse Mills ratio }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
