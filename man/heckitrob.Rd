\name{heckitrob}
\alias{heckitrob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust Heckit Fit
}
\description{
Fits the sample selection model using a robust two-stage estimator
}
\usage{
heckitrob(selection, outcome, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selection}{
formula, the selection equation
}
  \item{outcome}{
formula, the outcome equation
}
  \item{control}{
a list of parameters for controlling the fitting process
}
}
\details{
Compute robust two-stage estimates of the Heckman's selection model. The robust probit is fitted in the first stage. In the second stage the Mallows type M-estimator is used. 
The values of the tuning constants and the robustness weights can be modified in \code{\link{heckitrob.control}}.
}
\value{
Object of class "heckitrob".
%%  If it is a LIST, use
  \item{stage1}{object of class \code{glmrob} that contains robust probit fit}
  \item{stage2}{object of class \code{rlm} that contains second stage robust fit}
  \item{vcov}{variance matrix of the second stage}
  \item{call}{the matched call}
  \item{method}{method of estimation, currently only "robust two-stage" is implemented}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Cameron, C.A., Trivedi, P.K. (2009) Microeconometrics Using Stata. College Station, TX: Stata Press. 

Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827.
}
\author{
Mikhail Zhelonkin, Marc G. Genton, Elvezio Ronchetti
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[robustbase]{glmrob}}, \code{\link[MASS]{rlm}}, \code{\link{ssmrob}}, \code{\link{heckitrob.control}}, \code{\link{heckit5rob}}, \code{\link{etregrob}}
}
\examples{
# Zhelonkin, Genton, Ronchetti (2016): page 823
data(MEPS2001)
attach(MEPS2001)
selectEq <- dambexp ~ age + female + educ + blhisp + totchr + ins
outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
meps.fit <- ssmrob(selectEq, outcomeEq, control = heckitrob.control(tcc = 3.2))
summary(meps.fit)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample selection }
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line
