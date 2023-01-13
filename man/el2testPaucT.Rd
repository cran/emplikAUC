\name{el2testPaucT}
\alias{el2testPaucT}
\title{
Testing one pAUC(0, p) value and one quantile: F(tau) = 1-p together by Empirical Likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: pAUC(0, p) = theta and F(tau) = 1-p. (F is the CDF of X).
The two samples data are in the x-vector and y-vector inputs. It uses EM.
}
\usage{
el2testPaucT(tau, pauc, ind, partial, x, y, epsxy, epsT)
}
\arguments{
\item{tau}{The "true" value of the (1-p)-th quantile of X-distribution F, to be tested.}
\item{pauc}{The \eqn{H_0} value of pAUC(0, p) to be tested.}
\item{ind}{A smoothed indicator function, to generate a Matrix of (smoothed) indicator values: I[x[i] < y[j]].}
\item{partial}{The probability p in pAUC(0, p); also the p in F(tau) = 1-p.}
\item{x}{a vector of observations, length m, for the first sample, test-results with healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample, test-results with desease subjects.}
\item{epsxy}{The smoothing parameter when compare x-y.}
\item{epsT}{The smoothing parameter when compare x to Tau, for quantile estimation.}
}
\details{
This function is called by \code{el2testPauc( )}.  
It is listed here stand alone because users may find it useful elsewhere.

It make use of function \code{smooth3( )} and the function \code{el2.cen.EMm( )} 
from the \code{emplik2} package.

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
It returns one value that is the "-2LLR". It should have chi square df=2 under \eqn{H_0}.
}
\references{
Zhao, Y., Ding, X. and Zhou (2021). Confidence Intervals of AUC and pAUC by Empirical Likelihood.
Tech Report. https://www.ms.uky.edu/~mai/research/eAUC1.pdf
}
\author{
Mai Zhou <maizhou@gmail.com>.
}
\examples{ 
y <- c(10, 209, 273, 279, 324, 391, 566, 785)
x <- c(21, 38, 39, 51, 77, 185, 240, 289, 524)
}
\keyword{nonparametric}
