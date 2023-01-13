lam2 <- function(u0, v0, indicmat1, indicmat2, tol.H0, start) {
m <- length(u0) 
n <- length(v0)
indic4u1 <- v0 %*% t(indicmat1)
indic4u2 <- v0 %*% t(indicmat2)

indic4v1 <- u0 %*% indicmat1
indic4v2 <- u0 %*% indicmat2

lamfun <- function(lam, indic4u1, indic4u2, indic4v1, indic4v2, 
                indicmat1, indicmat2){
u <- 1/(m + lam[1] * indic4u1 + lam[2] * indic4u2)
v <- 1/(n + lam[1] * indic4v1 + lam[2] * indic4v2)
return(c(u%*%indicmat1%*%t(v), u%*%indicmat2%*%t(v)) ) }

lam <- multiroot(lamfun, start=start, maxiter=300, atol = tol.H0, 
indic4u1 = indic4u1, indic4u2 = indic4u2,
indic4v1 = indic4v1, indic4v2 = indic4v2,
indicmat1 = indicmat1, indicmat2 = indicmat2)$root

u <- 1/(m + lam[1] * indic4u1 + lam[2] * indic4u2)
v <- 1/(n + lam[1] * indic4v1 + lam[2] * indic4v2)
list(u=u/sum(u), v=v/sum(v), lambda=lam)
}



