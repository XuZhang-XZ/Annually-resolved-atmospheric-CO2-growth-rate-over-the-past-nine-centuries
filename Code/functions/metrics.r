
## Metrics for evaluating performance

## NSE
NSE=function(obs,pre){
  rows=which(!is.na(obs)&!is.na(pre))
  obs=obs[rows]
  pre=pre[rows]
  NSE=1-sum((obs-pre)^2,na.rm = TRUE)/sum((obs-mean(obs,na.rm=TRUE))^2,na.rm=TRUE)
  return(NSE)
}

RMSE=function(obs,pre){
  rows=which(!is.na(obs)&!is.na(pre))
  obs=obs[rows]
  pre=pre[rows]
  RMSE=(mean((obs-pre)^2,na.rm = TRUE))^0.5
  return(RMSE)
}

## RE
RE=function(obs,pre,calibration){
  rows=which(!is.na(obs)&!is.na(pre))
  obs=obs[rows]
  pre=pre[rows]
  calibration.mean=mean(calibration,na.rm=TRUE)
  NSE=1-sum((obs-pre)^2,na.rm = TRUE)/sum((obs-calibration.mean)^2,na.rm=TRUE)
  return(NSE)
}
