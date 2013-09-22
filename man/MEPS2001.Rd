\name{MEPS2001}
\alias{MEPS2001}
\docType{data}
\title{
Ambulatory Expenditures Data
}
\description{
Sample of 3328 observations, with 526 zero expenditures.
}
\usage{data(MEPS2001)}
\format{
  A data frame with 3328 observations on the following 22 variables.
  \describe{
    \item{\code{educ}}{education status}
    \item{\code{age}}{age}
    \item{\code{income}}{income}
    \item{\code{female}}{gender}
    \item{\code{vgood}}{a numeric vector}
    \item{\code{good}}{a numeric vector}
    \item{\code{hospexp}}{a numeric vector}
    \item{\code{totchr}}{number of chronic diseases}
    \item{\code{ffs}}{a numeric vector}
    \item{\code{dhospexp}}{a numeric vector}
    \item{\code{age2}}{a numeric vector}
    \item{\code{agefem}}{a numeric vector}
    \item{\code{fairpoor}}{a numeric vector}
    \item{\code{year01}}{a numeric vector}
    \item{\code{instype}}{a numeric vector}
    \item{\code{ambexp}}{ambulatory expenditures}
    \item{\code{lambexp}}{log ambulatory expenditures}
    \item{\code{blhisp}}{ethnicity}
    \item{\code{instype_s1}}{a numeric vector}
    \item{\code{dambexp}}{dummy variable, ambulatory expenditures}
    \item{\code{lnambx}}{a numeric vector}
    \item{\code{ins}}{insurance status}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
2001 Medical Expenditure Panel Survey by the Agency for Healthcare Research and Quality.
}
\references{
Cameron, C.A. and Trivedi, P.K. (2009) \emph{Microeconometrics Using Stata}, Stata Press.
}
\examples{
data(MEPS2001)
attach(MEPS2001)
hist(lnambx)
}
%\keyword{datasets}
