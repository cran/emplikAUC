\name{eltest4paucT}
\alias{eltest4paucT}
\title{
Testing one pAUC and one quantile together by Empirical Likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: pAUC(0,p) = theta and F(tau) = 1-p. 
The two samples are in the x-vector and y-vector.
}
\usage{
eltest4paucT(tau, x, y, true, ind, epsxy, epsT, tol.u, tol.v, tol.H0, p)
}
\arguments{
\item{tau}{The "true" value of the (1-p)th quantile of X, under \eqn{H_0}, to be tested.}
\item{x}{a vector of observations, length m, for the first sample. Test-results with healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample. Test-results with desease subjects.}
\item{true}{The \eqn{H_0} value of the pAUC(0, p) to be tested.}
\item{ind}{A smoothed indicator function, to generate a Matrix of (smoothed) indicator values: I[x[i] < y[j]].}
\item{epsxy}{Window width for smoother (ind) when compare x-y.}
\item{epsT}{Window width for smoother (ind) when find quantile.}
\item{tol.u}{Error tol for final u probability vector. Must > 0.}
\item{tol.v}{Error tol for final v probability vector. Must > 0.}
\item{tol.H0}{The error bound for checking if the constrained NPMLE satisfy H0, must >0.}
\item{p}{The probability p in pAUC(0, p), and also in F(tau) = 1-p.}
}
\details{
This function is similar to \code{el2testPaucT( )}. Just a different algorithm (not EM). 
Speed and convergence may be slightly different.

This function is called by \code{eltest4paucONE}.  
It is listed here because the user may find it useful elsewhere.

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list containing
\item{lambda}{The final tilting parameter.}
\item{u}{the new u vector.}
\item{v}{The new v vector.}
\item{"-2LLR"}{The -2 log empirical likelihood ratio.}
\item{iterNum}{The iteration number used in computing.}
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
