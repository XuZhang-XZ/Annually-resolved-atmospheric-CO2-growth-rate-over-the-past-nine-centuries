

## This function is used for conducting bootstrap of correlation coefficients

bootstrap.cor=function(cols=c(1,2),data.series){
  
  pr=0
  pr.p=0
  
  for(ge in 1:1000){
    
    ## data
    total.rows=c(1:NROW(data.series))
    sam.rows=sample(total.rows,NROW(data.series),replace=TRUE)
    data1=data.series[sam.rows,]
    
    ## correlation
    temp=cor.test(data1[,cols[1]],data1[,cols[2]])
    pr[ge]=temp$estimate
    pr.p[ge]=temp$p.value
  }
  
  kk=quantile(pr,probs=c(0.025,0.25,0.5,0.75,0.975))
  kk
  
}




bootstrap.pr=function(cols=c(1,2),data.series){
  
  pr=0
  pr.p=0
  
  for(ge in 1:1000){
    
    ## data
    total.rows=c(1:NROW(data.series))
    sam.rows=sample(total.rows,NROW(data.series),replace=TRUE)
    data1=data.series[sam.rows,]
    
    ## correlation
    temp=pcor.test(data1[,cols[1]],data1[,cols[2]],data1[,-cols[1:2]])
    pr[ge]=temp$estimate
    pr.p[ge]=temp$p.value
  }
  
  kk=quantile(pr,probs=c(0.025,0.975))
  kk1=quantile(pr.p,probs=c(0.95))
  
  return(c(kk,kk1))
}



## functions
## convert to polygon
conver_ploy=function(plot.GSFZ){
  plot.GSFZ1=plot.GSFZ
  plot.GSFZ1$bian.lon=plot.GSFZ$lon-plot.GSFZ$lon.width
  plot.GSFZ1$bian.lat=plot.GSFZ$lat-plot.GSFZ$lat.width
  
  plot.GSFZ2=plot.GSFZ
  plot.GSFZ2$bian.lon=plot.GSFZ$lon-plot.GSFZ$lon.width
  plot.GSFZ2$bian.lat=plot.GSFZ$lat+plot.GSFZ$lat.width
  
  plot.GSFZ3=plot.GSFZ
  plot.GSFZ3$bian.lon=plot.GSFZ$lon+plot.GSFZ$lon.width
  plot.GSFZ3$bian.lat=plot.GSFZ$lat+plot.GSFZ$lat.width
  
  plot.GSFZ4=plot.GSFZ
  plot.GSFZ4$bian.lon=plot.GSFZ$lon+plot.GSFZ$lon.width
  plot.GSFZ4$bian.lat=plot.GSFZ$lat-plot.GSFZ$lat.width
  
  plot.GSFZ5=rbind(plot.GSFZ1,plot.GSFZ2,plot.GSFZ3,plot.GSFZ4)
  return(plot.GSFZ5)
}