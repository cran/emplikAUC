eltest4diff2auc <- function(theta, x, y, ind, tol.u, tol.v, tol.H0)
{
##### "theta" is the diff value, AUC1 - AUC2, you are testing.
##### The input x should be a mx2 matrix, and y a nx2 matrix. 
##### ind is a (smoothed) indicator function, like smooth3(x,y)~I[y>x],
##### or no smooth, like smooth0(x,y). etc. smooth0 = I[y>x] + 0.5 I[x=y]

  nx <- dim(x)[1]
  ny <- dim(y)[1]
  
  u0 <- rep(1/nx, nx)
  v0 <- rep(1/ny, ny)
  
  indicmat <- ind(x[,1], y[,1]) - ind(x[,2], y[,2]) ### OK
  indicmatT <- indicmat - theta

  result <- New.el2.test(u0, v0, indicmatT, tol.H0)
  
  ustar <- 1 / (nx + result$lam * (indicmatT)%*%matrix(result$v, ncol=1) )
  vstar <- 1 / (ny + result$lam * matrix(result$u, nrow=1)%*%(indicmatT) )
  
  error.u <- sum(abs(ustar - matrix(result$u, ncol=1)))
  error.v <- sum(abs(vstar - matrix(result$v, nrow=1)))
  
  maxit <- 1
  while( ( (error.u > tol.u) & (maxit<39) ) || 
                       ( (error.v > tol.v) & (maxit<39) ) ) 
  {
    u <- result$u
    v <- result$v
    
    result <- New.el2.test(u, v, indicmatT, tol.H0)
    
   ustar <- 1/(nx + result$lam * (indicmatT)%*%matrix(result$v, ncol=1))
   vstar <- 1/(ny + result$lam * matrix(result$u, nrow=1)%*%(indicmatT))
    
    error.u <- sum(abs(ustar - matrix(result$u, ncol=1)))
    error.v <- sum(abs(vstar - matrix(result$v, nrow=1)))
    maxit <- maxit +1
  }
  
  stat <- -2*(sum(log(nx*result$u))+sum(log(ny*result$v)))
  pval <- pchisq(stat, df=1, lower.tail=F)
  
  list(lambda=result$lam, u=result$u, v=result$v, '-2LLR'=stat,
               Pval=pval, Maxiter=maxit)
}