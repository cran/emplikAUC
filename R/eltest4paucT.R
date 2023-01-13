eltest4paucT <- function(tau, x, y, true, ind, epsxy, epsT, tol.u, tol.v, tol.H0, p){
#####  true: the H0 value of pAUC to be tested. pAUC(0, p) that is.
#####  tau:  the H0 value of quantile of Fx (i.e. F(tau) = p) to be tested.  
nx <- length(x)
ny <- length(y)
u0 <- rep(1/nx, nx)
v0 <- rep(1/ny, ny)
tauVec <- rep(tau, ny)

TauMat0 <- t(ind(tauVec,x, eps=epsT))
TauMat <- TauMat0 - p                    ### use a different eps?
PaucMat <- ind(x,y, eps=epsxy)*TauMat0 - true

result <- lam2(u0, v0, TauMat, PaucMat, tol.H0, start=c(0,0))

ustar <- 1 / (nx + 
result$lam[1] * TauMat %*% matrix(result$v, ncol=1) +
result$lam[2] * PaucMat %*% matrix(result$v, ncol=1))


vstar <- 1 / (ny +
result$lam[1] * matrix(result$u, nrow=1) %*% TauMat +
result$lam[2] * matrix(result$u, nrow=1) %*% PaucMat)

error.u <- sum(abs(ustar-matrix(result$u, ncol=1)))
error.v <- sum(abs(vstar-matrix(result$v, nrow=1)))

iterN <- 1
while( (error.u > tol.u) || (error.v > tol.v)  && (iterN < 30)) {
      u <- result$u
      v <- result$v
      lamOLD <- result$lam
      result <- lam2(u, v, TauMat, PaucMat, tol.H0, start=lamOLD/(31-iterN))
      #### can we start at the last root? instead of (0,0)??
      ustar <- 1 / (nx + 
      result$lam[1] * TauMat %*% matrix(result$v, ncol=1) +
      result$lam[2] * PaucMat %*% matrix(result$v, ncol=1))
      vstar <- 1 / (ny +
      result$lam[1] * matrix(result$u, nrow=1) %*% TauMat +
      result$lam[2] * matrix(result$u, nrow=1) %*% PaucMat)
      error.u=sum(abs(ustar-matrix(result$u, ncol=1)))
      error.v=sum(abs(vstar-matrix(result$v, nrow=1)))
      iterN <- iterN + 1
}
stat <- -2*(sum(log(nx*result$u))+sum(log(ny*result$v)))
###pval <- pchisq(stat, df=1, lower.tail=F)  #### df =1? or 2??
list(lambda=result$lam, u=result$u, v=result$v, "-2LLR"=stat, IterNum=iterN)
}

