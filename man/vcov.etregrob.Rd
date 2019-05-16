\name{vcov.etregrob}
\alias{vcov.etregrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract Asymptotic Variance Covariance Matrix
}
\description{
Extracts the variance covariance matrix of the robust endogenous treatment model fit
}
\usage{
\method{vcov}{etregrob}(object, ...)
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
Variance covariance matrix of the second stage. Variance covariance matrix of the selection stage can be extracted using the \code{vcov} function for the probit estimator, e.g. \code{vcov(etregrob.object$stage1)}.
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

\seealso{
\code{\link{etreg2steprobVcov}}
}
