\name{smoothLvec}
\alias{smoothLvec}
\title{
Smoothed indicator function I[x < const], linear connecting the 1 and 0 on [-eps, eps].  
}
\description{
This function computes the smoothed Indicator function I[x < const] using
a liare.

If |x - const| > eps then the result is the same as the indicator function I[x < const] (either 0 or 1).
For |x - const| < eps, it is a linear function.

\eqn{eps} is a scalar, must > 0. It is the smoothing window width.
}
\usage{
smoothLvec(x, const, eps=0.05)
}
\arguments{
\item{x}{a vector of observations, length m, for the first sample.}
\item{const}{a single number.}
\item{eps}{The smoothing window width, must be >0. Ideally this should be sample size dependent.}
}
\details{
This function is similar to \code{smooth3vec} but only compare the x vector to a single number 
and thus returns a vector instead of matrix. 
You may also use the \code{smooth3()} with a bit care, for that matter, but this vector
version should be faster and save memory.  
It is listed here because the user may find it useful elsewhere.

We used this function to estimate the quantile from the x-sample.
}
\value{
\code{smoothLvec} returns a vector of length=length(x). The entry
of the vector are smoothed values of  I[x[i] < const].
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
