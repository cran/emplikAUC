\name{emplikAUC.internal}
\alias{lamONE}
\alias{lam2}
\title{Internal emplikAUC functions}
\description{
These are internal functions called by \code{eltest4aucONE} and \code{eltest4paucT}.  They are not intended
 to be called by users.
}
\usage{
lamONE(u0, v0, indicmat, tol.H0)
lam2(u0, v0, indicmat1, indicmat2, tol.H0, start)
}

\arguments{
\item{u0}{a vector of probabilities, length m, for the first (X) sample.}
\item{v0}{a vector of propabilities, length n, for the second (Y) sample.}
\item{indicmat}{a Matrix (size m by n) of indicator function values: I[X[i] < Y[j]], But typically smoothed and centered.}
\item{tol.H0}{The error bound for (checking) if satisfy \eqn{H_0}, must > 0.}
}

\value{ 
Output is a list containing:
\item{u}{the new u vector.}
\item{v}{the new v vector.}
\item{lam}{The solution lambda.}
}


\details{
\code{lamONE()} find the lambda (tilting parameter) in the two sample Empirical Likelihood, 
with an AUC constrain. 

This function computes the lambda that satisfy null hypothesis \eqn{H_0} constraint by
using \code{uniroot}.

This function is called by \code{eltest4aucONE}.  
It is listed here because the user may find it useful elsewhere.


The function \code{lam2}, similar to function \code{lamONE},
calculates the tilting parameters (lam1, lam2) for the empirical likelihood of
a joint hypothesis about pAUC(0, p) = theta and F(tau) = p.

The \code{indicmat1}, \code{indicmat2} should reflect the joint hypothesis, in the U-stat fashion.

The function \code{lam2( )} is called by \code{eltest4paucT( )}, and is 
similar to \code{lamONE}
except there are 2 lambda's and needs
to call \code{multiroot()} to solve the
equation. 

And also, with a starting value input to help the 
iterations of solving the equation. It (start) is directly passed to \code{multiroot( )} function.

Author: Ding Xue.
}
\keyword{internal}
