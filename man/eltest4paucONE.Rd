\name{eltest4paucONE}
\alias{eltest4paucONE}
\title{
Testing one pAUC value by Empirical likelihood. 
}
\description{
This function computes the two sample Log Empirical Likelihood ratio
for testing \eqn{H_0}: pAUC(0, p) = theta. The two samples are in the x-vector and y-vector.
}
\usage{
eltest4paucONE(theta,x,y,nuilow,nuiup,ind,partial,epsxy=0.05,epsT=(length(x))^(-0.75))
}
\arguments{
\item{theta}{The "true" value of the pAUC under \eqn{H_0}, to be tested.}
\item{x}{a vector of observations, length m, for the first sample. Test-results with healthy subjects.}
\item{y}{a vector of observations, length n, for the second sample. Test-results with desease subjects.}
\item{nuilow}{The lower bound for the nuisance parameter (the (1-p)-th quantile of X CDF F) search.}
\item{nuiup}{The upper bound for the nuisance parameter search.}
\item{ind}{A smoothed indicator function, to generate a Matrix of (smoothed) indicator values: I[x[i] < y[j]].}
\item{partial}{The probability p in the pAUC(0,p).}
\item{epsxy}{Window width for the smoother, "ind", when compare x-y.}
\item{epsT}{Window width for the smoother, "ind", when define quantile.}
}
\details{
This function calls the function \code{eltest4paucT}.
We then use \code{optimize( )} to profile out the nuisance parameter.
Return an empirical likelihood ratio siutable for testing one parameter pAUC(0,p).

The empirical likelihood we used here is defined as
\deqn{ EL = \prod_{i=1}^m v_i \prod_{j=1}^n \nu_j ~;~~~~~~ \sum v_i =1 ~,~~ \sum \nu_j =1 ~. }
}
\value{
A list containing
\item{"-2LLR"}{The -2 log empirical likelihood ratio.}
\item{Nupar}{The nuisance parameter value to achieve the minimum.}
\item{Pval}{The p-value.}
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
