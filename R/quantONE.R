quantONE <- function(x, prob, myeps=(length(x))^(-0.75)) {
##################################################################
##### When call this function like in
##### quantONE(x=Xis, prob=0.2, myeps=(length(x))^(-0.75))
##### will give error (x not found). You must call it as
#####    quantONE(x=Xis, prob=0.2, myeps=(length(Xis))^(-0.75))
##### But, it defauts to the myeps works. The default value may be too
##### large, and may consider myeps = 0.01*(length(x))^(-0.75). See
##### Chen and Hall (1992).
##################################################################
if( (prob <= 0) || (prob >= 1) ) stop("Input prob must > 0 and < 1") 

   tempQfun <- function(tau, x, prob, myeps){
                 sum(smooth3vec(x=x, const=tau, eps=myeps))/length(x) - prob
                }
uplow <- quantile(x, prob=c(prob-0.03, prob+0.03), type=9)
resu <- uniroot(tempQfun, lower=uplow[1], upper=uplow[2], extendInt="yes",
                x=x, prob=prob, myeps=myeps, tol=.Machine$double.eps^0.5)
### if( prob == 0 ) resu$root <- Inf   #??? test needed
return(resu$root)
}

##### Depend on smooth3vec() #####