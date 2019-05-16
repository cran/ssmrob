\name{coef.heckitrob}
\alias{coef.heckitrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Coefficients from Robust Sample Selection Model Fit
}
\description{
Returns the coefficients of the robust two-stage estimator for simple Heckman's selection model or switching regression model.
}
\usage{
\method{coef}{heckitrob}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class "\code{\link{heckitrob}}" or "\code{\link{heckit5rob}}"
}
  \item{\dots}{
currently not used
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a list of two (censored) or three (switching) vectors of parameters of two stages.
%%  If it is a LIST, use
  \item{S }{coefficients of the selection equation.}
  \item{O }{coefficients of the outcome equation(s).}
%% ...
}
%\references{
%Heckman (1979), Zhelonkin, Genton and Ronchetti (2012).
%}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%\code{\link{sampleSelection}}
%}

% R documentation directory.
\keyword{ M-estimator }
\keyword{ Robust two-stage }
\keyword{ Switching regressions }
% __ONLY ONE__ keyword per line
