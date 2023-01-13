\name{el2testPauc}
\alias{el2testPauc}
\title{
Testing one pAUC(0, p) value by Empirical likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: pAUC(0, p) = theta. 
The two samples are in the x-vector and y-vector inputs.
}
\usage{
el2testPauc(theta, x, y, ind, nuilow, nuiup, partial, epsxy, epsT)
}
\arguments{
\item{theta}{The "true" value of the pAUC(0, p) under \eqn{H_0}, to be tested.}
\item{x}{a vector of observations, length m, for the first sample, test-results with the healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample, test-results with the desease subjects.}
\item{ind}{The (smoothed) indicator function for compare x-y.}
\item{nuilow}{Lower bound for the nuisamce parameter (1-p)-th quantile of X) search.}
\item{nuiup}{Upper bound for nuisance parameter search.}
\item{partial}{The probability p in pAUC(0, p).}
\item{epsxy}{The smoothing parameter when compare x-y.}
\item{epsT}{The smoothing parameter when calculating quantile.}
}
\details{
This function will call another function: \code{el2testPaucT( )}.
We then use \code{optimize( )} to profile out the nuisance 
parameter tau: the (1-p)-th quantile of X distribution.

Can be used by  \code{findUnew( )} etc.

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~; ~~~~s.t. ~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list containing
\item{"-2LLR"}{The -2 log empirical likelihood ratio.}
\item{Nupar}{The nuisance parameter value that achieved the minimum.}
\item{Pval}{The p-value, by using chi square distribution with 1 df.}
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
