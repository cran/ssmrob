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
heckitrob(selection, outcome, data, control = heckitrob.control())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{selection}{
formula, the selection equation
}
  \item{outcome}{
formula, the outcome equation
}
  \item{data}{
an optional data frame containing the variables in the model. If not found in data, the variables are taken from \code{environment(formula)}, typically the environment from which \code{heckitrob} is called.
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
  \item{coefficients}{a named vector of coefficients}
  \item{stage1}{object of class \code{glmrob} that contains robust probit fit}
  \item{stage2}{object of class \code{rlm} that contains second stage robust fit. Note that the standard errors in this object are biased, since they are not corrected for the uncertainty in the first estimation step. Use \code{vcov} below}
  \item{vcov}{variance matrix of the second stage}
  \item{sigma}{the standard error of the error term of the outcome equation}
  \item{IMR1}{inverse Mills ratio for the case when y_1=1}
  \item{call}{the matched call}
  \item{method}{method of estimation, currently only "robust two-stage" is implemented}
  \item{converged}{logical. Did all the estimators converge?}
  \item{iterations}{list containing the numbers of iterations}
%% ...
}
\references{
Cameron, C.A., Trivedi, P.K. (2009) Microeconometrics Using Stata. College Station, TX: Stata Press. 

Heckman, J.J. (1979) Sample Selection Bias as a Specification Error. \emph{Econometrica}, 47, p. 153-161.

Zhelonkin, M., Genton M.G., and Ronchetti, E. (2016) Robust Inference in Sample Selection Models. \emph{Journal of the Royal Statistical Society, Series B}, 78, p. 805-827. \doi{10.1111/rssb.12136}

Zhelonkin, M., Ronchetti, E. (2021) Robust Analysis of Sample Selection Models through the R Package ssmrob. \emph{Journal of Statistical Software}, 99, 4, p. 1-35. \doi{10.18637/jss.v099.i04}
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
selectEq <- dambexp ~ age + female + educ + blhisp + totchr + ins
outcomeEq <- lnambx ~ age + female + educ + blhisp + totchr + ins
meps.fit <- ssmrob(selectEq, outcomeEq, data = MEPS2001, control = heckitrob.control(tcc = 3.2))
summary(meps.fit)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Sample selection }
\keyword{ Robust estimator }% __ONLY ONE__ keyword per line
