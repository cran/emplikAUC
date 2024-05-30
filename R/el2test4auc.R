el2test4auc <- function(theta, x, y, ind){
el2.cen.EMs(x=x, dx=rep(1,length(x)), y=y, dy=rep(1,length(y)), 
            fun=ind, mean=theta, maxit=30)
} 

###### This function used to be called  ThetafunAUC(). 
###### There was also a function called ThetafunAUCone() but later removed.
###### Both got cut and we have instead el2test4auc()/eltest4aucONE().
###### We can use findUnew() etc. on either of el2test4auc()/eltest4aucONE().
###### The function name with --ONE at the end, uses the XD algorithm,
###### without --ONE, uses empilik2 package (EM based).