

## This function is used for calculating area-weighted average

areal.mean=function(z,A.Weight){
  dim.year = dim(z)[3]
  x1 = NA
  for(i in 1:dim.year){
    x1[i] = sum(z[,,i] * A.Weight, na.rm = TRUE)/sum(is.finite(z[,,i]) * A.Weight)
  }
  x1
}

areal.mean_sum = function(z,A.Weight){
  dim.year = dim(z)[3]
  x1 = NA
  for(i in 1:dim.year){
    x1[i] = sum(z[,,i] * A.Weight, na.rm = TRUE)
  }
  x1
}

area.drought=function(z,A.Weight){
  dim.year = dim(z)[3]
  x1 = NA
  for(i in 1:dim.year){
    x1[i] = sum(z[,,i] * A.Weight, na.rm = TRUE)
  }
  x1
}


Areal=function(z,A.Area,startv,endv){
  
  z1=z
  z1[which(!(z1>=startv&z1<=endv))]=NA
  z1[which(!is.na(z1))]=1
  
  z.area <- sum(z1 * A.Area, na.rm = TRUE)
  if(is.nan(z.area)) {z.area=0}
  z.area
}
