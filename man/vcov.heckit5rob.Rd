\name{vcov.heckit5rob}
\alias{vcov.heckit5rob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Asymptotic Variance Covariance Matrix
}
\description{
Extract the variance covariance matrix of the robust sample selection model fit
}
\usage{
\method{vcov}{heckit5rob}(object, ...)
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
%\details{
%
%}
\value{
Variance covariance matrix of the second stage. Variance covariance matrix of the selection stage can be extracted using the \code{vcov} function for the probit estimator, e.g. \code{vcov(heckitrob.object$stage1)}.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%\references{
%}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%bla-bla%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{heck2steprobVcov}}
}
%\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
%}


