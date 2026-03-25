
## These function are used for calculating correlations with different outputs and checks of NAs

cor7=function(a1,a2){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=7) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}


cor30=function(a1,a2){
  
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=30) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2,method = c("pearson"))
  return(cor)
}

cor25.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=25) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}

cor30.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=30) {return(c(NA,NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}

cor50.1=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  total.data=total.data[complete.cases(total.data),]
  if(NROW(total.data)<=50) {return(list("estimate"=NA,"p.value"=NA))}
  cor=cor.test(total.data$a1,total.data$a2)
  return(c(cor$estimate,cor$p.value))
}


cor.non.na=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  if(length(which(is.na(total.data)))>0) {return(list("estimate"=NA,"p.value"=NA))}
  
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}
cor.non.na2=function(a1,a2){
  total.data=data.frame("a1"=a1,"a2"=a2)
  if(length(which(is.na(total.data)))>2) {return(list("estimate"=NA,"p.value"=NA))}
  
  cor=cor.test(total.data$a1,total.data$a2)
  return(cor)
}

