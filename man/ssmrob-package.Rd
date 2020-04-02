\name{ssmrob-package}
\alias{ssmrob-package}
%\alias{ssmrob}
\docType{package}
\title{
Robust Estimation and Inference in Sample Selection Models
}
\description{
Package provides a set of tools for robust estimation and inference for
models with sample selectivity.
}
\details{
\tabular{ll}{
Package: \tab ssmrob \cr
Type: \tab Package \cr
Version: \tab 0.8 \cr
Date: \tab 2020-04-01 \cr
License: \tab GPL-2 \cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti

Maintainer: Mikhail Zhelonkin <Mikhail.Zhelonkin@gmail.com>
}
\references{
Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827. \doi{10.1111/rssb.12136}
}
\keyword{ Robust estimation }
\keyword{ Sample selection }
\keyword{ Endogenous treatment }
\seealso{
\code{\link{ssmrob}}, \code{\link{etregrob}}, \code{\link{heckitrob}}, \code{\link{heckit5rob}}, \code{\link[sampleSelection]{selection}}
}
\examples{
# Zhelonkin, Genton, Ronchetti (2016): page 823
data(MEPS2001)
selectEq <- dambexp ~ age + female + educ + blhisp + totchr + ins
outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
meps.fit <- ssmrob(selectEq, outcomeEq, data = MEPS2001, control = heckitrob.control(tcc = 3.2))
summary(meps.fit)
}

