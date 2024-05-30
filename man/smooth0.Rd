\name{smooth0}
\alias{smooth0}
\title{
Non-Smoothed indicator function 0.5I[x=y] + I[x < y].  
}
\description{
This function computes the non-smoothed Indicator function 0.5I[x=y] + I[x < y].

}
\usage{
smooth0(x, y)
}
\arguments{
\item{x}{a vector of observations, length m, for the first sample.}
\item{y}{a vector of observations, length n, for the second sample.}
}
\details{
This function is used in the original AUC computation (non-smoothed). 
You may achieve the same result with a (data dependent) small eps in the function smooth3(x,y,eps). 
But this is faster and no eps to worry about.
It is listed here because users may find it useful.

}
\value{
\code{smooth0( )} returns a matrix of dimension \code{ncol=length(y)}, \code{nrow=length(x)}. The entry
of the matrix are values of  0.5I[x[i] = y[j]] + I[x[i] < y[j]].
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
