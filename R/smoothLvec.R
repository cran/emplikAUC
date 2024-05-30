#####  Linear smoothing vector version, added 2/2023.
smoothLvec <- function(x, const, eps=0.05) {
      if (eps <= 0) stop("eps must > 0")
      if (length(const) > 1)  stop("const must be of length one")
         x <- as.vector(x)
         Dc <- (x - const)

         DcDeps <- Dc/eps
         INDE <- (DcDeps <1) & (DcDeps > -1)
         u <- DcDeps
         u[ DcDeps >= 1 ] <- 0
         u[ DcDeps <= -1 ] <- 1
         v <- 0.5 - u[INDE]/2
         u[INDE] <- v
return(u)
}
