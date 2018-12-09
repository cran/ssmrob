\name{etreg2steprobVcov}
\alias{etreg2steprobVcov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variance Covariance Matrix
}
\description{
Computation of the asymptotic variance matrix of the robust Heckman's two-stage estimator for endogenous treatment model.
}
\usage{
etreg2steprobVcov(y1vec, y2vec, x1Matr, x2Matr, eststage1, eststage2, eststage2sigma, 
				  weights = rep(1, nrow(y1vec)), t.c = 1.345)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y1vec}{
vector of endogenous variables of the selection stage
}
  \item{y2vec}{
vector of endogenous variables of the outcome stage
}
  \item{x1Matr}{
matrix of exogenous variables of the selection stage
}
  \item{x2Matr}{
matrix of exogenous variables of the outcome stage
}
  \item{eststage1}{
object of class "\code{glmrob}", corresponding to the robust probit fit
}
  \item{eststage2}{
vector of the coefficients of the outcome stage
}
  \item{eststage2sigma}{
the robust scale estimate of the second stage regression
}
  \item{weights}{
robustness weights 
}
  \item{t.c}{
tuning constant of the second stage
}
}
\details{
The computation is made using the Huber (1967) - White (1980) sandwich estimator.  In the computation of leverage weights the lambda's are assumed to be fixed.
}
\value{
Variance covariance matrix of the second stage estimator
}
\references{
Huber, P.J. (1967) The Behavior of Maximum Likelihood Estimates under Nonstandard Conditions. \emph{ Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability; L.M. LeCam, J. Neyman (Eds.), Berkeley: University of California Press}, p. 221-233.

White, H.J. (1980) A Heteroskedasticity-Consistent Covariance Matrix Estimator and a Direct Test for Heteroskedasticity. \emph{Econometrica}, 48, p. 817-838.
}
\author{
Mikhail Zhelonkin
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\keyword{ Asymptotic variance }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
