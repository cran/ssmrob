\name{MEPS2001}
\alias{MEPS2001}
\docType{data}
\title{
Ambulatory Expenditures Data
}
\description{
The dataset is an extract from the Medical Expenditures Panel Survey. Sample of 3328 observations, with 526 zero expenditures. It was used for illustrative examples by Cameron and Trivedi (2009, Chapter 16).
}
\usage{data(MEPS2001)}
\format{
  A data frame with 3328 observations on the following 12 variables.
  \describe{
    \item{\code{educ}}{Educational attainment, in years.}
    \item{\code{age}}{Age.}
    \item{\code{income}}{Income.}
    \item{\code{female}}{Logical, TRUE if female.}
    \item{\code{totchr}}{Number of chronic diseases.}
    \item{\code{age2}}{Age squared}
    \item{\code{ambexp}}{Ambulatory expenditures.}
    \item{\code{lambexp}}{Log ambulatory expenditures.}
    \item{\code{blhisp}}{Ethnicity. Logical, TRUE if black or hispanic.}
    \item{\code{dambexp}}{Logical, TRUE if the ambulatory expenditures are greater than zero.}
    \item{\code{lnambx}}{Log ambulatory expenditures, with zeros replacing NA's.}
    \item{\code{ins}}{Insurance status. Logical, TRUE if insured.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
\source{
2001 Medical Expenditure Panel Survey by the Agency for Healthcare Research and Quality.
}
\references{
Cameron, C.A. and Trivedi, P.K. (2009) \emph{Microeconometrics Using Stata}, College Station, TX: Stata Press.
}
\examples{
data(MEPS2001)
summary(MEPS2001)
}
%\keyword{datasets}
