\name{quantONE}
\alias{quantONE}
\title{
Smoothed quantile estimation from the given x-sample.  
}
\description{
This function computes the smoothed quantile estimate, 
using the smoothing function \code{smooth3}. (or it can be changed easily)
Otherwise it is similar to R function \code{quantile()}.
Compare to \code{quantile(x, prob, type=9)}.

\eqn{myeps} is a scalar, must > 0. It is the smoothing window width.
}
\usage{
quantONE(x, prob, myeps=(length(x))^(-0.75))
}
\arguments{
\item{x}{a vector of observations, length m, the sample.}
\item{prob}{a probabilty.}
\item{myeps}{The smoothing window width, must >0.}
}
\details{
This function is called by \code{myEstPaucT}.  
It is listed here because the user may find it useful elsewhere.

Known problems: when the input \code{prob} is too close to 0 or 1 (distance less than 0.03),
the computation will stop.
Also, if the solution to the equation
\deqn{ \tau ~| ~~~ \frac{1}{m} \sum_{i=1}^m smooth3vec(X_i, \tau, myeps)  =  prob } 
is not unique, this function only returns one of the solutions.

}
\value{
The function \code{quantONE} returns a scalar that is the estimated (prob)-th quantile of X distribution.
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
#### To estimate the 70-th percentile of x distrubution:
quantONE(x=x, prob=0.7)
#### we should get 239.9474. 
}
\keyword{nonparametric}
