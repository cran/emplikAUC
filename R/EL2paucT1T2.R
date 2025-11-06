######   Used to make inference of pAUC(p1, p2) and tauVec   ######
EL2paucT1T2 <- function(tauVec, pauc, partial1, partial2, x, y, epsxy){
nx <- length(x)            ####  tauVec=(tau1, tau2), and must have: tau1 < tau2 and       ###
ny <- length(y)            ####  tau1 =F^{-1}(1-partial2); tau2 =F^{-1}(1-partial1)
dx <- rep(1, nx)           ####  thus, must have    partial2 > partial1
dy <- rep(1, ny)

H11 <- smooth3(x=x, y=y, eps=epsxy)
H12 <- as.matrix(1-smooth3(x=x, y=rep(tauVec[1], ny), eps=nx^(-0.75)) )
H13 <- as.matrix(  smooth3(x=x, y=rep(tauVec[2], ny), eps=nx^(-0.75)) )
H1 <- H11*H12*H13 - pauc
H2 <- H12 - partial2 
H3 <- H13 - (1-partial1)
Hmat <- cbind(H1, H2, H3)

el2.cen.EMm(x=x, dx=dx, y=y, dy=dy, p=3, H=Hmat, mean=c(0,0,0), maxit=30)$"-2LLR"
}

##### depend on the package emplik2. 