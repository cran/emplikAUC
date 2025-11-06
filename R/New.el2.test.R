New.el2.test <- function(u0, v0, indicmat, tol.H0, step=0.001) 
{
  m <- length(u0)
  n <- length(v0)
  
  indic4u <- v0 %*% t(indicmat)
  indic4v <- u0 %*% indicmat
  
  #du <- 0.02 * m /abs(sum(indic4u))  ### why 0.02??
  #dv <- 0.02 * n /abs(sum(indic4v))
  #dd <- min(du, dv)
  dd <- step

  lamfun <- function(lam, indic4u, indic4v, indicmat) {
    u <- 1/(m + lam * indic4u)
    v <- 1/(n + lam * indic4v)
    return(u %*% indicmat %*% t(v))
  }
  
  lamzero <- lamfun(0, indic4u, indic4v, indicmat)
  Diff <- lamfun(dd,indic4u,indic4v,indicmat) -
                       lamfun(-dd,indic4u,indic4v,indicmat)

  if ( abs(lamzero) < tol.H0 ) { lam <- 0 }
  else {
     if( (lamzero > 0) & (Diff > 0) ) {
         temp2 <- uniroot(lamfun, lower = -dd, upper = 0, 
                      extendInt="upX", tol = tol.H0, 
                      indic4u = indic4u, indic4v = indic4v, 
                      indicmat = indicmat)
        }
     if( (lamzero > 0) & (Diff < 0) ) {
         temp2 <- uniroot(lamfun, lower = 0, upper = dd,
                     extendInt="downX", tol = tol.H0, 
                     indic4u = indic4u, indic4v = indic4v, 
                     indicmat = indicmat)
        }
     if( (lamzero < 0) & (Diff > 0) ) {
       temp2 <- uniroot(lamfun, lower = 0, upper = dd, 
                     extendInt="upX", tol = tol.H0, 
                     indic4u = indic4u, indic4v = indic4v, 
                     indicmat = indicmat)
        }
     if( (lamzero < 0) & (Diff < 0)  )  {
     temp2 <- uniroot(lamfun, lower = -dd, upper = 0, 
                       extendInt="downX", tol = tol.H0, 
                       indic4u = indic4u, indic4v = indic4v, 
                       indicmat = indicmat)
          }
     lam <- temp2$root
   }
  u1 <- 1/(m + lam * indic4u)
  v1 <- 1/(n + lam * indic4v)
  list(u = u1/sum(u1), v = v1/sum(v1), lam = lam)
}