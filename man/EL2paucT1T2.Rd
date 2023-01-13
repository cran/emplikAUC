\name{EL2paucT1T2}
\alias{EL2paucT1T2}
\title{
Testing one pAUC(p1, p2) and two quantile values together by Empirical Likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: pAUC(p1, p2) = theta; F(tau1)=1-p2; F(tau2)= 1-p1. 
The two samples are in the x-vector and y-vector.
}
\usage{
EL2paucT1T2(tauVec, pauc, partial1, partial2, x, y, epsxy)
}
\arguments{
\item{tauVec}{The vector (tau1, tau2), the two quantile values, to be tested.}
\item{pauc}{The value of the pAUC(p1, p2) under \eqn{H_0}, to be tested.}
\item{partial1}{The probability that define the quantile 1}
\item{partial2}{The probability that define the quantile 2. Must satisfy partial 1 < partial 2 .}
\item{x}{a vector of observations, length m, for the first sample.}
\item{y}{a vector of observations, length n, for the second sample.}
\item{epsxy}{The parameter for smoothing when compare x-y.}
}
\details{
The input \code{tauVec=(tau1, tau2)}, and must have: tau1 < tau2. The relavant definitions are: 
tau1 = \eqn{F^{-1}}(1-partial2); tau2 = \eqn{F^{-1}}(1-partial1).
Thus, we must have  partial2 > partial1.

This function is testing 3 parameters simultanuously.
It depend on the package \code{emplik2}.

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~s.t. ~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A single value that is the "-2LLR" from \code{emplik2::el2.cen.EMm()}.
Typically should be distributed as chi square df=3, under \eqn{H_0}.
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
