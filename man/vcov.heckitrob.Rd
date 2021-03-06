\name{vcov.heckitrob}
\alias{vcov.heckitrob}
\title{
Extract Asymptotic Variance Covariance Matrix
}
\description{
Extract the variance covariance matrix of the robust sample selection model fit
}
\usage{
\method{vcov}{heckitrob}(object, ...)
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



