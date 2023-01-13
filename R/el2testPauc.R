el2testPauc <- function(theta, x, y, ind, nuilow, nuiup, partial, epsxy, epsT) {
######  Testing the H0: pAUC(0, partial) = theta  (One parameter)
######  nuilow/nuiup  are the search interval for the tau (nuisance parameter)
######  the nuisance parameter tau := quantile of Fx; F(tau) = partial. 
temp <- optimize(f=el2testPaucT, interval=c(nuilow, nuiup), pauc=theta, 
                 partial=partial, x=x, y=y, ind=ind, epsxy=epsxy,epsT=epsT)
cstar <- temp$minimum
val <- temp$objective
list("-2LLR"=val, Nupar=cstar, Pval=1-pchisq(val, df=1) )
}

##### depend on el2testPaucT( ). Can be used by  findULNEW( ). 