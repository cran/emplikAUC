findUnew <- function(step=0.01,initStep=0,fun,MLE,level=qchisq(0.95,df=1), tol=.Machine$double.eps^0.5,...)
{
tempfun <- function(beta){return(level-fun(beta,...)$"-2LLR")}

Ubeta <- MLE + initStep
Ubeta0 <- Ubeta + 3*step
Ubeta1 <- Ubeta
temp2 <- uniroot(tempfun,lower=Ubeta1,upper=Ubeta0, extendInt="downX", tol=tol)
Ubeta <- temp2$root
value <- level-temp2$f.root
return(list(Up=Ubeta, FstepU=temp2$estim.prec, Uvalue=value))
}
