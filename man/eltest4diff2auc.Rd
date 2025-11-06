\name{eltest4diff2auc}
\alias{eltest4diff2auc}
\title{
Testing the Difference of 2 AUC values, Paired Data, by Empirical likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: AUC1-AUC2 = theta. The two samples of paired data 
are in the x-matrix(mx2) and y-matrix(nx2).
}
\usage{
eltest4diff2auc(theta, x, y, ind, tol.u, tol.v, tol.H0)
}
\arguments{
\item{theta}{The "true" value of the AUC under \eqn{H_0}, to be tested.}
\item{x}{a matrix of observations, dim mx2, for the first sample. The test-results of biomarker 1 and 2 on
          healthy subjects}
\item{y}{a matrix of observations, dim nx2, for the second sample. The test-results of biomarker 1 and 2 on
          desease subjects.}
\item{ind}{A (smoothed) indicator function, to generate a Matrix of (smoothed) indicator values: I[x[i] < y[j]].}
\item{tol.u}{Error tol for final u probability vector. Must > 0.}
\item{tol.v}{Error tol for final v probability vector. Must > 0.}
\item{tol.H0}{The error bound for the constrained NPMLE to satisfy \eqn{H_0}, must >0.}
}
\details{
This function is similar to \code{el2test4auc}, but for the difference of 2 AUCs.
Using our own algorithm (not EM).  
We listed 3 kind of tol to control convergence.

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list containing
\item{lambda}{The final tilting parameter.}
\item{u}{the new u vector.}
\item{v}{The new v vector.}
\item{"-2LLR"}{The -2 log empirical likelihood ratio.}
\item{Pval}{The p-value.}
\item{Maxiter}{The iteration number used in computing.}
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
#### We know the AUC estimator here is 0.75.
#### We may test a hypothesis about the AUC value: H0: AUC= 0.7
eltest4aucONE(theta=0.7, x=x, y=y, ind=smooth3, tol.u=1e-6, tol.v=1e-6, tol.H0=1e-6)
#### Two of the outputs should be '-2LLR'=0.1379561 and Pval=0.7103214
}
\keyword{nonparametric}
