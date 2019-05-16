\name{MROZ.RAW}
\alias{MROZ.RAW}
\docType{data}
\title{
Wage Offer Data
}
\description{
Sample of 753 observations, with 325 truncated observations.
}
\usage{data(MROZ.RAW)}
\format{
  A data frame with 753 observations on the following 22 variables.
  \describe{
    \item{\code{inlf}}{in labor force (binary) }
    \item{\code{hours}}{hours worked}
    \item{\code{kidslt6}}{number of young children}
    \item{\code{kidsge6}}{number of children greater than 6 years of age}
    \item{\code{age}}{age}
    \item{\code{educ}}{education status}
    \item{\code{wage}}{wage}
    \item{\code{repwage}}{a numeric vector}
    \item{\code{hushrs}}{a numeric vector}
    \item{\code{husage}}{husband's age}
    \item{\code{huseduc}}{husband's education}
    \item{\code{huswage}}{husband's wage}
    \item{\code{faminc}}{family income}
    \item{\code{mtr}}{a numeric vector}
    \item{\code{motheduc}}{mother's education}
    \item{\code{fatheduc}}{a numeric vector}
    \item{\code{unem}}{a numeric vector}
    \item{\code{city}}{a numeric vector}
    \item{\code{exper}}{experience}
    \item{\code{nwifeinc}}{non-wife income}
    \item{\code{lwage}}{log-wage}
    \item{\code{expersq}}{squared experience}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Mroz , T.A. (1987) The Sensitivity of an Empirical Model of Married Women's Hours of Work to Economic and Statistical Assumptions. \emph{Econometrica}, 55, p. 765-799.

Wooldridge, J.M. (2002) \emph{Econometric Analysis of Cross Section and Panel Data}, Cambridge MA: MIT Press.
}
\examples{
data(MROZ.RAW)
attach(MROZ.RAW)
hist(lwage)
}
%\keyword{datasets}
