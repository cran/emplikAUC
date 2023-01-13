\name{myEstPaucT}
\alias{myEstPaucT}
\title{
Given the x, y 2-sample data, 
first estimate the (1-partial)-th quantile of X sample, then 
estimate the pAUC(0, partial), with the plug-in estimated quantile.
}
\description{
This function computes the estimator using smoothed Indicator function I[x < y] which is
a 3rd order polynomial.

\eqn{eps} is a scalar, must > 0. It is the smoothing window width for indicator function when compare x-y.
\eqn{epsT} is a scalar, must > 0. It is the smoothing window width for estimating quantile function when compare x with tau.
}
\usage{
myEstPaucT(x, y, partial, eps=0.05, epsT=(length(x))^(-0.75)) 
}
\arguments{
\item{x}{a vector of observations, length m, for the first sample. Test-results with healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample. Test-results with desease subjects.}
\item{partial}{The probability in pAUC(0, partial). }
\item{eps}{The smoothing window width, for indicator I[x < y]. Must >0.}
\item{epsT}{The smoothing window width for the quantile estimation. Must >0.}
}
\details{
This function gives the estimators that are consistant with our smoothing used in the computation of 
empirical likelihood.
Typically the smoother for quantile should be a bit "smoother" that usual.

Known problem: when input \code{partial} is too close to 1 or 0, it will fail.
When \code{partial} is equal to 1, then the pAUC reduces to AUC, which can be estimated easily,
as the example below shows.

Why not also give the AUC estimator? May be it is too easy. See example below.
}
\value{
This function returns a list, with 
\item{"tau(1-partial)"}{The estimator for the (1-partial)-th quantile of X-distribution,} and
\item{"Pauc(0, partial)"}{The estimated pAUC.} 
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
#### To get an estimator of the AUC (not pAUC), using our smooth3( ), we do
sum(smooth3(x=x, y=y))/(length(x)*length(y))
#### We should get AUC = 0.75.
#### To get the estimator of the pAUC(0, 0.3) AND the 70th quantile of x sample
myEstPaucT(x=x, y=y, partial=0.3)
#### We should get estimator tau(1-0.3)=239.9474 and pAUC(0, 0.3)=0.1416667.
}
\keyword{nonparametric}
