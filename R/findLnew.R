findLnew <- function(step=0.01,initStep=0,fun,MLE,level=qchisq(0.95,df=1), tol=.Machine$double.eps^0.5,...)
{
Lbeta <- MLE - initStep
################################
## Improved by Mai Zhou 10/10/2021 by using "extendInt" of uniroot( ).
## The result is that it is not sensitive to step value (in terms of computing time). 
## We can use a smaller step= value without slowing down much.
################################
Lbeta0 <- Lbeta - 3*step
Lbeta1 <- Lbeta
tempfun <- function(beta){return(level-fun(beta,...)$"-2LLR")}

temp1 <- uniroot(tempfun,lower=Lbeta0,upper=Lbeta1, extendInt="upX", tol=tol)
Lbeta <- temp1$root
value1 <- level-temp1$f.root

return(list(Low=Lbeta, FstepL=temp1$estim.prec, Lvalue = value1))
}

#####  Also, finding Upper bound and Lower bound are made into two separate functions
#####  Because, some nuisance parameter could have different range when compute
#####  Upper or Lower confidence bounds.   #######
