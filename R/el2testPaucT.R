###### For testing a single pAUC(0, p) and a single tau, using emplik2 package #####
el2testPaucT <- function(tau, pauc, ind, partial, x, y, epsxy, epsT){
nx <- length(x)
ny <- length(y)
dx <- rep(1, nx)
dy <- rep(1, ny)

H11 <- ind(x=x, y=y, eps=epsxy)
H12 <- as.matrix(1-ind(x=x, y=rep(tau, ny), eps=epsT)) 
H1 <- H11*H12 - pauc
H2 <- H12 - partial 
Hmat <- cbind(H1, H2)

el2.cen.EMm(x=x, dx=dx, y=y, dy=dy, p=2, H=Hmat, mean=c(0,0), maxit=25)$"-2LLR"
}

###### The same test function but using DX algorithm is called eltest4paucT(). ####
###### The output -2LLR should have 2 DF.