\name{New.el2.test}
\alias{New.el2.test}
\title{
One Iteration of Computing Paired Two Sample Constrained NPMLE. 
}
\description{ 
This function computes the two sample Constrained Data NPMLE: u vectoe and v vector.
}
\usage{
New.el2.test(u0, v0, indicmat, tol.H0, step=0.001)
}
\arguments{
\item{u0}{The constrained NPMLE from initial probability for sample one.}
\item{v0}{given constrained NPMLE, initial probability for sample two}
\item{indicmat}{A Matrix of (smoothed) indicator values: I[x[i] < y[j]] - theta.}
\item{tol.H0}{a positive number for solve constrained NPMLE tolerance.}
\item{step}{a positive number. Used to approximate the dirivative of lambda function at 0.}
}
\details{
The constraint is specified by matrix \code{indicmat} in 
\deqn{ u \%*\% indicmat \%*\% t(v) = 0 }

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list that is the same as el2.cen.EMs() from emplik2 package.
Which contains
\item{u}{The new constrained NPMLE. X sample.}
\item{v}{The new constrained NPMLE. Y sample.}
\item{lam}{The lambda value.}
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
