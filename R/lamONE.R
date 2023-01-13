lamONE <- function(u0, v0, indicmat, tol.H0){
############################################################
## Improved by Mai Zhou at 10/10/2021 by using the extendInt option of
## uniroot( ). This improves computing time for testing H0 values that 
## are far from MLE, i.e. lamfun(0, ...) is far from 0. 
## Up to 80 times faster.
############################################################
m <- length(u0)
n <- length(v0)
indic4u <- v0%*%t(indicmat)
indic4v <- u0%*%indicmat
du <- 0.02*m/abs(sum(indic4u))
dv <- 0.02*n/abs(sum(indic4v))
dd <- min(du, dv)
lamfun <- function(lam, indic4u, indic4v, indicmat){
                   u <- 1/(m+lam*indic4u)
                   v <- 1/(n+lam*indic4v)
                   return(u%*%indicmat%*%t(v))}

if(abs(lamfun(0, indic4u, indic4v, indicmat)) < tol.H0){lam <- 0}
else{
     if(lamfun(0, indic4u, indic4v, indicmat) > 0){
     temp <- uniroot(lamfun, lower=0, upper=dd, extendInt="downX", tol=tol.H0, 
                     indic4u=indic4u, indic4v=indic4v, indicmat=indicmat)
     }
     else{
          temp <- uniroot(lamfun, lower= -dd, upper=0, extendInt="downX", tol=tol.H0,
                          indic4u=indic4u, indic4v=indic4v, indicmat=indicmat)
         }
lam <- temp$root 
}
u1=1/(m + lam * indic4u)
v1=1/(n + lam * indic4v)
list(u = u1/sum(u1), v = v1/sum(v1), lam = lam)
}

