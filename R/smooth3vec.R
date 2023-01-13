smooth3vec <- function (x, const, eps = 0.05) {
         if (eps <= 0) stop("eps must > 0")
         if (length(const) > 1)  stop("const must be of length one")
         x <- as.vector(x)

         tempD <- (x - const)
         uu <- tempD/eps
         INDE <- (uu < 1) & (uu > -1)

         uu[uu >= 1] <- 0
         uu[uu <= -1] <- 1
         vv <- 0.5 + 0.75 * uu[INDE] - uu[INDE]^3/4
         uu[INDE] <- 1 - vv
         return(uu)
}

#####  The vector version of smooth3(). The output is a vector of  
#####   of length (x) ; instead of a matrix.    ####