\name{el2test4auc}
\alias{el2test4auc}
\title{
Testing one AUC value by Empirical likelihood. 
}
\description{
By calling upon a function from the package \code{emplik2} (using EM), 
this function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: AUC = theta. The two samples are in the x-vector and y-vector inputs.
}
\usage{
el2test4auc(theta, x, y, ind)
}
\arguments{
\item{theta}{The "true" value of the AUC under \eqn{H_0}, to be tested.}
\item{x}{a vector of observations, length m, for the first sample. The test-results of healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample. The test-results of desease subjects.}
\item{ind}{A smoothed indicator function, to generate a Matrix of (smoothed) indicator values: I[x[i] < y[j]].}
}
\details{
This function is similar to the function \code{eltest4aucONE()}, the difference 
is that we call the function \code{emplik2::el2.cen.EMs()} to 
do the heavy computation (instead of by our own code).  
So, the speed and convergence property may be slightly different. When they both converge
the results should be the same.


The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list that is the same as el2.cen.EMs() from emplik2 package.
Which contains
\item{lambda}{The final tilting parameter.}
\item{"-2LLR"}{The -2 log empirical likelihood ratio.}
\item{Pval}{The p-value.}
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
#### The estimation of AUC
sum(smooth3(x=x, y=y))/(length(x)*length(y))
#### This does not work in Rcmd check: (truncate at %*%) 
####   rep(1/length(x), length(x))%*%smooth3(x=x, y=y)%*%rep(1/length(y), length(y))
#### The result should be 0.75.
#### We may then test a hypothesis about the AUC value: H0: AUC= 0.7
el2test4auc(theta=0.7, x=x, y=y, ind=smooth3)
#### Two of the outputs should be '-2LLR'=0.1379561 and Pval=0.7103214
}
\keyword{nonparametric}
