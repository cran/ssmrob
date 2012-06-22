\name{heckitrob.control}
\alias{heckitrob.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Auxiliary for Controlling Robust Fitting
}
\description{
Auxiliary function used for fitting the sample selection models. Contains control sequences, tuning constants and robustness weight functions.
}
\usage{
heckitrob.control(acc = 1e-04, test.acc = "coef", maxit = 50, tcc = 1.345, 
                    weights.x1 = c("none", "hat", "robCov", "covMcd"), 
                    weights.x2 = c("none", "hat", "robCov", "covMcd"), t.c = 1.345)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acc}{
positive convergence level
}
  \item{test.acc}{
Only "coef" is currently implemented
}
  \item{maxit}{
integer giving the maximum number of iterations
}
  \item{tcc}{
tuning constant c for Huber's psi-function for the selection stage
}
  \item{weights.x1}{
robustness weights controlling for the leverage effect in the selection equation
}
  \item{weights.x2}{
robustness weights controlling for the leverage effect in the outcome equation
}
  \item{t.c}{
tuning constant c for Huber's psi-function for the outcome stage
}
}
%
\value{
A list with the arguments as components.
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
\keyword{ Tuning constant }
\keyword{ Robust weights }% __ONLY ONE__ keyword per line
