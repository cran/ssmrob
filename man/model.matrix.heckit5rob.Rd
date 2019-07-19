\name{model.matrix.heckit5rob}
\alias{model.matrix.heckit5rob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Design Matrix of Switching Regression Model
}
\description{
Create design matrix of switching regression model
}
\usage{
\method{model.matrix}{heckit5rob}(object, part = "outcome", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class \code{heckit5rob}
}
  \item{part}{
character string indicating which matrix to extract: "outcome" for the outcome equations, "selection" for the selection equation.
}
  \item{\dots}{
currently not used
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
If argument \code{part} is \code{"outcome"}, the list containing design matrices of the outcome equations is returned.
\item{stage21}{design matrix for the first regime}
\item{stage22}{design matrix for the second regime}
If argument \code{part} is \code{"selection"}, the design matrix of the selection equation is returned.
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
%        return(list(model.matrix(object$stage21), model.matrix(object$stage22)))
%    else if (part == "selection") 
%        return(model.matrix(object$stage1))
%    else cat("Please choose either 'outcome' or 'selection' matrix")
%  }
%}
%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
