smooth3 <- function (x, y, eps = 0.05) {
	if (eps <= 0) stop("eps must > 0")

	tempD <- as.vector(outer(x, y, FUN = "-"))
	uu <- tempD/eps
	INDE <- (uu < 1) & (uu > -1)
	uu[uu >= 1] <- 0
	uu[uu <= -1] <- 1
	vv <- 0.5 + 0.75 * uu[INDE] - uu[INDE]^3/4
	uu[INDE] <- 1 - vv
	return(matrix(uu, ncol = length(y), nrow = length(x), byrow = FALSE))
}