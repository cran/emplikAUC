eltest4aucONE <- function(theta, x, y, ind, tol.u, tol.v, tol.H0){  
##### modify: first input named theta, was called "true". 12/5/2021
##### theta is the H0 value of the AUC to be tested. 
##### This new naming is to satisfy "fingULnew()" function. 
true <- theta          #### theta is the true value (H0 value) we are testing
nx <- length(x)        ##### set smoothing eps in the ind()
ny <- length(y)
u0 <- rep(1/nx, nx)
v0 <- rep(1/ny, ny)
aucMat <- ind(x,y) - true
result <- lamONE(u0, v0, aucMat, tol.H0)
ustar <- 1/(nx + result$lam * aucMat%*%matrix(result$v, ncol=1))
vstar <- 1/(ny + result$lam * matrix(result$u, nrow=1)%*%aucMat)
error.u <- sum(abs(ustar-matrix(result$u, ncol=1)))
error.v <- sum(abs(vstar-matrix(result$v, nrow=1)))
iterN <- 1     ### added by Mai Zhou 10/9/2021
while( ((error.u > tol.u) || (error.v > tol.v)) && (iterN < 30)){
      u=result$u
      v=result$v
      result=lamONE(u, v, aucMat, tol.H0)
      ustar=1/(nx + result$lam * aucMat%*%matrix(result$v, ncol=1))
      vstar=1/(ny + result$lam * matrix(result$u, nrow=1)%*%aucMat)
      error.u=sum(abs(ustar-matrix(result$u, ncol=1)))
      error.v=sum(abs(vstar-matrix(result$v, nrow=1)))
      iterN <- iterN +1 }
stat <- -2*(sum(log(nx*result$u))+sum(log(ny*result$v)))     #### can I use ustar/vstar???
pval <- pchisq(stat, df=1, lower.tail=F)
list(lambda=result$lam, u=result$u, v=result$v, "-2LLR"=stat, "Pval"=pval, iterNum=iterN)
}
