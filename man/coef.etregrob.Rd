\name{coef.etregrob}
\alias{coef.etregrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Coefficients from Robust Endogenous Treatment Model Fit
}
\description{
Returns the coefficients of the robust two-stage estimator for endogentous treatment model.
}
\usage{
\method{coef}{etregrob}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class "\code{\link{etregrob}}"
}
  \item{\dots}{
currently not used
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a list of two vectors of parameters of two stages.
%%  If it is a LIST, use
  \item{S }{coefficients of the selection equation.}
  \item{O }{coefficients of the outcome equation.}
%% ...
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Mikhail Zhelonkin
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
%function (object, ...) 
%{
%    coeff <- list()
%    coeff$S <- coef(object$stage1)
%    coeff$O <- coef(object$stage2)
%    class(coeff) <- c("coef.etregrob", class(coeff), class(coeff$S))
%    return(coeff)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
