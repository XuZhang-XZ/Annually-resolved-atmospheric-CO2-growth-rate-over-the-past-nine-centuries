
## This function is used for calculating correlations for variables with SD > 0

cor.test1=function(a1,a2,method){
  
  if(sd(a1)==0|sd(a2)==0){
    kk=list("estimate"=0,"p.value"=0.8)
  } else {
    kk=cor.test(a1,a2,method=method)
  }
  kk
}

