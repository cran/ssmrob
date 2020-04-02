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
    \item{\code{inlf}}{Logical, TRUE if in labor force.}
    \item{\code{hours}}{Hours worked by wife in 1975.}
    \item{\code{kidslt6}}{Number of young children, 5 year or younger.}
    \item{\code{kidsge6}}{Number of children greater than 6 years of age.}
    \item{\code{age}}{Wife's age.}
    \item{\code{educ}}{Education level in years.}
    \item{\code{wage}}{Average hourly earnings in 1975.}
    \item{\code{repwage}}{Wife's wage reported at the time of the 1976 interview.}
    \item{\code{hushrs}}{Husband's hours worked in 1975.}
    \item{\code{husage}}{Husband's age.}
    \item{\code{huseduc}}{Husband's education in years.}
    \item{\code{huswage}}{Husband's wage in 1975.}
    \item{\code{faminc}}{Family income in 1975.}
    \item{\code{mtr}}{Marginal tax rate facing the wife.}
    \item{\code{motheduc}}{Wife's mother's education in years.}
    \item{\code{fatheduc}}{Wife's father's education in years.}
    \item{\code{unem}}{Unemployment rate in county of residence.}
    \item{\code{city}}{Logical, TRUE if live in large city.}
    \item{\code{exper}}{Wife's labor market experience in years.}
    \item{\code{nwifeinc}}{Non-wife income, households's total money minus wife's labor income.}
    \item{\code{lwage}}{Wife's log-wage.}
    \item{\code{expersq}}{Wife's squared experience.}
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
summary(MROZ.RAW)
}
%\keyword{datasets}
