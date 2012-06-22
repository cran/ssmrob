\name{coef.heckit5rob}
\alias{coef.heckit5rob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Coefficients from Robust Sample Selection Model
}
\description{
Returns the coefficients of the robust two-stage estimator for simple Heckman's selection model or switching regression model.
}
\usage{
\method{coef}{heckit5rob}(object, ...)
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
%\code{\link{sampleSelection}}
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ M-estimator }
\keyword{ Robust two-stage }
\keyword{ Switching regressions }

