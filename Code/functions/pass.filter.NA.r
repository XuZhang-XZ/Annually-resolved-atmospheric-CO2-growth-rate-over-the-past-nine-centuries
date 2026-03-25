

## High-pass filter 

pass.filter.NA=function(a=raw.chro[,2],W=70,type="high",method = c("Butterworth")){
  a1=a
  V.ge=which(!is.na(a))
  if(length(V.ge)<=15) {return(array(NA,length(a)))}
  
  a1[V.ge]=dplR::pass.filt(y=a[V.ge],W=W,type=type,method = method)
  a1
}
 
## High-pass filter after moving average

pass.filter.NA.mean=function(a,W=70,type="high",method = c("Butterworth"),n = 4, Rp = 1){
  
  a1=a
  V.ge=which(!is.na(a))
  
  if(length(V.ge)<=21) {return(array(NA,length(a)))}
  
  a1[V.ge]= dplR::pass.filt(y=a[V.ge],W=W,type=type,method = method,n = n, Rp = Rp)
  a1=a1-mean(a1,na.rm=TRUE)
  a1
} 
