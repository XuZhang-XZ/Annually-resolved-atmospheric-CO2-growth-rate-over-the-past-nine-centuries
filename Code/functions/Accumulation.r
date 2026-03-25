

## This function is used for calculating accumulation

Accumulation=function(a) {
  a.accu=a
  for(i in 2:length(a.accu)){
    temp = a[i] + a.accu[i-1]
    a.accu[i] = ifelse(is.na(temp), 0, temp)
  }
  a.accu[which(is.na(a))]=NA
  a.accu[1]=NA
  a.accu
}
