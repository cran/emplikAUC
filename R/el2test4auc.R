el2test4auc <- function(theta, x, y, ind){
el2.cen.EMs(x=x, dx=rep(1,length(x)), y=y, dy=rep(1,length(y)), 
            fun=ind, mean=theta, maxit=30)
} 

###### This function used to be called  ThetafunAUC(). 
###### There was also a function called ThetafunAUCone() but later removed.
###### Both got cut and we have instead eltest4auc()/eltest4aucONE().
###### We can use findUL() on either of eltest4auc()/eltest4aucONE().
###### The function name with --ONE at the end, use the XD algorithm,
###### without --ONE, uses empilik2 package.