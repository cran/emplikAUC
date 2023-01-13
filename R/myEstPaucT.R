myEstPaucT <- function(x, y, partial, eps=0.05, epsT=(length(x))^(-0.75)){
###############################################
#### partial must > 0 and < 1              ####
#### eps must > 0; epsT must > 0.
#### Depend on smooth3(), quantONE().
###############################################
  nx <- length(x)
  ny <- length(y)

  tauEst <- quantONE(x=x, prob=1-partial, myeps=epsT)

  H11 <- smooth3(x=x, y=y, eps=eps)
  H12 <- as.matrix(1-smooth3(x=x, y=rep(tauEst, ny), eps=epsT))
    ####  in the future, change smooth3 to smooth3vec. No need to rep(tauEst,.)###
  H1 <- H11*H12
  PaucEst <- as.numeric(rep(1/nx, nx)%*% H1 %*%rep(1/ny, ny))
list("tau(1-partial)"=tauEst, "Pauc(0,partial)"=PaucEst)
}
