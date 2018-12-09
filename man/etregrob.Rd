\name{etregrob}
\alias{etregrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Fit of Endogenous Treatment Model
}
\description{
Fits the endogenous treatment model using a robust two-stage estimator
}
\usage{
etregrob(outcome, selection, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{outcome}{
formula, the outcome equation
}
  \item{selection}{
formula, the selection equation
}
  \item{control}{
a list of parameters for controlling the fitting process. The same list as for sample selection model
}
}
\details{
Function provides a robust two-step estimator. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimator is used instead of traditional OLS. The correction for endogeneity is made by means of control function, which is the inverse Mills ratio for a complete sample (see Maddala, 1983, p. 120-122).
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "etregrob".
}
\references{
Maddala G.S. (1983) Limited-Dependent and Qualitative Variables in Econometrics. Cambridge: Cambridge University Press.
}
\author{
Mikhail Zhelonkin
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
%\examples{
%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Endogenous treatment }% use one of  RShowDoc("KEYWORDS")
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line
