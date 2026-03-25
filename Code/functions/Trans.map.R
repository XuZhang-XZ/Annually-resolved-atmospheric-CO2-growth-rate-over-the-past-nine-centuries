
## Transformation Map -180-180 to 0-360

Trans.map = function(shp1){
  shp2 = data.frame()
  group.id = levels(unique(shp1$group))
  j=2
  for(j in 1:length(group.id)){
    
    pol.shp = shp1[shp1$group == group.id[j],]
    lon.r = range(pol.shp$long)
    
    if(lon.r[1]>=(0)) {
      shp2 = rbind(shp2,pol.shp)
      next
    }
    if(lon.r[2]<=(0)) {
      pol.shp$long = pol.shp$long+360
      shp2 = rbind(shp2,pol.shp)
      next
    }
    if(lon.r[1]<(0)&lon.r[2]>(0)) {
      id1 = paste0(group.id[j],".1")
      shp.temp1 = pol.shp
      shp.temp1$long[shp.temp1$long<(-1)] = -1
      shp.temp1$group = id1
      
      id2 = paste0(group.id[j],".2")
      shp.temp2 = pol.shp
      shp.temp2$long = shp.temp2$long+360
      shp.temp2$long[shp.temp2$long>(361)] = 361
      shp.temp2$group = id2
      
      shp2 = rbind(shp2,shp.temp1,shp.temp2)
    }
  }
  shp2
}