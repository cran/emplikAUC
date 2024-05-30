smooth0 <- function (x, y) {

    uu <- as.vector(outer(x, y, FUN = "-"))
    INDEX <- (uu == 0)
    uu[uu > 0] <- 0
    uu[uu < 0] <- 1
    uu[INDEX] <- 0.5
    return(matrix(uu, ncol = length(y), nrow = length(x), byrow = FALSE))
}

#### This function has no smoothing, i.e. = I[y>x] + 0.5 I[x=y] ####