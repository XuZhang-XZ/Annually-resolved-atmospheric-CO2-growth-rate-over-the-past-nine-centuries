
## These functions are used for detrending series

detrend.linear=function(x){
  
  if(length(which(is.na(x)))>(length(x)-5)) {
    return(NA)
  }
  
  data1=data.frame("ge"=c(1:length(x)),
                   "x"=x)
  # kk1=lm(x~ge+I(ge^2),data=data1)
  kk1=lm(x~ge,data=data1)
  fitted.valuess=predict(kk1,data1)
  detre.ser=x-fitted.valuess
  return(detre.ser)
}



detrend.filter=function(x,W){
  
  if(length(which(is.na(x)))>(length(x)-5)) {
    return(NA)
  }
  
  library(signal)
  x1=x[c(100:1,1:length(x),length(x):(length(x)-100+1))]
  
  bf <- butter(1, W=1/W, type="high")
  detre.ser <- filter(bf, x1)
  detre.ser = detre.ser[101:(100+length(x))]
  
  return(detre.ser)
}
