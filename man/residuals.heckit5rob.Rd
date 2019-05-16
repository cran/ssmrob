\name{residuals.heckit5rob}
\alias{residuals.heckit5rob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Residuals of Robust Sample Selection Model Fit}
\description{
Calculate the residuals of the sample selection model using robust fit.
}
\usage{
\method{residuals}{heckit5rob}(object, ...)
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
In case of truncated selection model one vector of residuals is returned. In case of switching regression model a list containing two vectors corresponding to two regimes is returned.
}
\value{
The numeric vector(s) of the residuals.
}

\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{heckitrob}}; \code{\link{heckit5rob}}
}



