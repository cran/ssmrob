\name{MmatrM}
\alias{MmatrM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
M Matrix
}
\description{
M matrix of a linear regression M-estimator of Mallows type.
}
\usage{
MmatrM(x, y, beta, sigma, t.c = 1.345, weights = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
matrix of explanatory variables
}
  \item{y}{
vector of dependent variables
}
  \item{beta}{
vector of parameters
}
  \item{sigma}{
robust scale estimate
}
  \item{t.c}{
tuning constant c for Huber's psi-function
}
  \item{weights}{
vector of robustness weights controlling for the leverage effects
}
}

\details{
Computes the M matrix of the M-estimator of Mallows type. In current implementation only the Huber score function is available.
}
\value{
M matrix for the sandwich formula.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
