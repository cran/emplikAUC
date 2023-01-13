\name{findLnew}
\alias{findLnew}
\title{
Finding the Lower bound of a confidence interval for theta by repeatedly testing the 
hypothesis for the parameter theta. 
}
\description{
This function try to find the Lower bound of a confidence interval 
by repeatedly testing the 
hypothesis for the parameter theta until we reach the 
\code{level} for the "-2LLR" which return from
the user supplied function \code{fun}. 

}
\usage{
findLnew(step=0.01, initStep=0, fun, MLE, level=qchisq(0.95,df=1), 
           tol=.Machine$double.eps^0.5,...)
}
\arguments{
\item{step}{Search step. Must > 0. }
\item{initStep}{The initial step from MLE. May be used if we knew the bound is far away from MLE. This help
to speed up things. Should be >=0.}
\item{fun}{The function that should return "-2LLR".}
\item{MLE}{The MLE of the parameter.}
\item{level}{The level of the confidence. Default to 3.84 which is 95 percent confidence.}
\item{tol}{The error bound for achieving the level given.}
\item{...}{Any additional input to be passed to fun.}
}
\details{
This function is similar to \code{findUnew( )}. 

}
\value{
It returns a list containing
\item{Low}{Lower bound of the confidence interval.}
\item{FstepL}{The error when search for Lower bound.}
\item{Lvalue}{The final likelihood ratio value for Lower bound. Should =~ level.}
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
