\name{smooth3}
\alias{smooth3}
\title{
Smoothed indicator function I[x < y], which is the integration of the Epanechnikov kernal.  
}
\description{
This function computes the smoothed Indicator function I[x < y] using
a 3rd order polynomial.

If |x-y| > eps then the result is the same as the indicator function I[x < y] (either 0 or 1).
For |x-y| < eps, it is a 3rd order polynomial.

\eqn{eps} is a scalar, must > 0. It is the smoothing window width.
}
\usage{
smooth3(x, y, eps=0.05)
}
\arguments{
\item{x}{a vector of observations, length m, for the first sample.}
\item{y}{a vector of observations, length n, for the second sample.}
\item{eps}{The smoothing window width, must >0.}
}
\details{
This function is used in many places to replace an indicator function \eqn{I[x<y]}. 
For example, when estimating the AUC. 
It is listed here because users may find it useful elsewhere.

}
\value{
\code{smooth3( )} returns a matrix of dimension \code{ncol=length(y)}, \code{nrow=length(x)}. The entry
of the matrix are smoothed values of  I[x[i] < y[j]].
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
