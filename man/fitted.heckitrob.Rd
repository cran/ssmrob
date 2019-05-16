\name{fitted.heckitrob}
\alias{fitted.heckitrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fitted values of robust sample selection model
}
\description{
Calculate the fitted values of the sample selection model using robust fit.
}
\usage{
\method{fitted}{heckitrob}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
object of class "\code{\link{heckitrob}}" or object of class "\code{\link{heckit5rob}}"
}
  \item{\dots}{
currently not used
}
}
\details{
In case of truncated selection model one vector of fitted values is returned. In case of switching regression model a list, that contains two vectors corresponding to two regimes, is returned.
}
\value{
vector of fitted values or a list that contains two vectors of fitted values
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
%
%## The function is currently defined as
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
