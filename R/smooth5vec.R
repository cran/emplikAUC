smooth5vec <- function (x, const, eps = 0.05) {
##### 1-CDF, 1-F(t) of the Quartic kernel, 
##### support on [const-eps, const+eps]. It has 2 cont. derivatives.

        if (eps <= 0) stop("eps must > 0")
        if (length(const) > 1)  stop("const must be of length one")
        x <- as.vector(x)
        

        tempD <- (x - const)
        uu <- tempD/eps
        INDE <- (uu < 1) & (uu > -1)

        uu[uu >= 1] <- 0
        uu[uu <= -1] <- 1
        vv <- 0.5 + (15/16)*(uu[INDE] - (2/3)*uu[INDE]^3 + uu[INDE]^5/5)
        uu[INDE] <- 1-vv
        return(uu)
}


