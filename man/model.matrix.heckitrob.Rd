\name{model.matrix.heckitrob}
\alias{model.matrix.heckitrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Design Matrix of Sample Selection Model
}
\description{
Create design matrix of sample selection model
}
\usage{
\method{model.matrix}{heckitrob}(object, part = "outcome", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class \code{heckitrob}
}
  \item{part}{
character string indicating which matrix to extract: "outcome" for the outcome equation, "selection" for the selection equation.
}
  \item{\dots}{
currently not used
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
If argument \code{part} is \code{"outcome"}, the design matrix of the outcome equation is returned.

If argument \code{part} is \code{"selection"}, the design matrix of the selection equation is returned.
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
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
%function (object, part = "outcome", ...) 
%{
%    if (part == "outcome") 
%        return(model.matrix(object$stage2))
%    else if (part == "selection") 
%        return(model.matrix(object$stage1))
%    else cat("Please choose either 'outcome' or 'selection' matrix")
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
