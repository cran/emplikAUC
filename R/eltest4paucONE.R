eltest4paucONE <- function(theta, x, y, nuilow, nuiup, ind, partial, epsxy=0.05, epsT=(length(x))^(-0.75)) {
#####
##### make the function only return one value, not a list.
tempfun <- function(tau,x,y,true,ind,epsxy,epsT,tol.u,tol.v,tol.H0,p){
       eltest4paucT(tau=tau,x=x,y=y,true=true,ind=ind,epsxy=epsxy,epsT=epsT,
	                         tol.u=tol.u,tol.v=tol.v,tol.H0=tol.H0,p=p)$"-2LLR"
	   }

temp <- optimize(f=tempfun, interval=c(nuilow, nuiup), x=x,y=y,
                 true=theta, ind=ind, epsxy=epsxy, epsT=epsT, 
				 tol.u=1e-05, tol.v=1e-05, tol.H0=1e-05, p=partial)
##### How to make these tol values flexible? more inputs??
cstar <- temp$minimum
val <- temp$objective
list("-2LLR"=val, Nupar=cstar, Pval=1-pchisq(val, df=1) )
}
