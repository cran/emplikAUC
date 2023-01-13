findULNEW <- function(step=0.01,initStep=0,fun,MLE,level=qchisq(0.95,df=1), tol=.Machine$double.eps^0.5,...)
{
value <- 0
step1 <- step
Lbeta <- MLE - initStep
#########################################
## Improved by Mai Zhou 10/10/2021 by using "extendInt" of uniroot( ).
## The result is that it is not time sensitive to step value. We can use
## a smaller step= value without slowing down too much.
#########################################
Lbeta0 <- Lbeta - 3*step1
Lbeta1 <- Lbeta
tempfun <- function(beta){ return(level-fun(beta,...)$"-2LLR") }

temp1 <- uniroot(tempfun,lower=Lbeta0,upper=Lbeta1, extendInt="upX", tol=tol)
Lbeta <- temp1$root

value1 <- level-temp1$f.root
value <- 0
Ubeta <- MLE+initStep

Ubeta0 <- Ubeta + 3*step1
Ubeta1 <- Ubeta
temp2 <- uniroot(tempfun,lower=Ubeta1,upper=Ubeta0, extendInt="downX", tol=tol)
Ubeta <- temp2$root
value <- level-temp2$f.root
return(list(Low=Lbeta,Up=Ubeta,FstepL=temp1$estim.prec, FstepU =temp2$estim.prec, Lvalue = value1, Uvalue = value))
}
