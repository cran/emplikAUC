\name{findULNEW}
\alias{findULNEW}
\title{
Finding the Upper and Lower bound of a confidence interval for theta by repeatedly testing the 
hypothesis for the parameter theta. 
}
\description{
This function try to find the Upper and Lower bound of a confidence interval 
by repeatedly testing the 
hypothesis for the parameter theta until we reach the 
given level for the "-2LLR" which is returned from
the user supplied function \code{fun}. 
}
\usage{
findULNEW(step=0.01, initStep=0, fun, MLE, level=qchisq(0.95,df=1), 
           tol=.Machine$double.eps^0.5,...)
}
\arguments{
\item{step}{Search step. Must > 0. }
\item{initStep}{The initial step from MLE. May be used if we knew the bound is far away from MLE. This help
to speed up things. Should be >=0.}
\item{fun}{The function that should return "-2LLR".}
\item{MLE}{The MLE of the parameter.}
\item{level}{The level of the confidence. Default to 3.84 which is 95 percent confidence (assume df=1).}
\item{tol}{The error bound for achieving the level given.}
\item{...}{Any additional input to be passed to fun.}
}
\details{
This function just combines the two functions \code{findUnew( )} and \code{findLnew( )}.
}
\value{
It returns a list containing
\item{Low}{Lower bound of the confidence interval.}
\item{Up}{Upper bound of the confidence interval.}
\item{FstepL}{The error when search for Lower bound.}
\item{FstepU}{The error when search for Upper bound.}
\item{Lvalue}{The final likelihood ratio value for Lower bound. Should = level.}
\item{Uvalue}{The final likelihood ratio value for Upper bound. Should = level.}
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
