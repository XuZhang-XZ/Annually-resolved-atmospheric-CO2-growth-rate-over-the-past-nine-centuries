

# Re-grid for CMIP6 Data ------------------------------------------------------------

## Functions
ks = 32
CMIP6_regrid = function(ks){
  
  ## Path
  .libPaths(new = "Z:/Software/R/Library",include.site = TRUE)
  
  ## Functions
  library(ncdf4)
  library(dplyr)
  library(tidyr)
  library(terra)
  library(lubridate)
  
  ## Data
  PMIP.models = read.csv("Output Data/PMIIP_past1000/PMIP.models.csv",row.names = 1)
  PMIP.group = PMIP.models%>%
    dplyr::group_by(model,varivant,Output.variable)%>%
    group_data()

  ## Select
  sel.rows = PMIP.group$.rows[[ks]]
  stats.temp = PMIP.models[sel.rows,]
  
  ## Test
  out.name = paste0(
    "Z:/2022/Science9_SM/Output Data/PMIIP_past1000/Gridded Data_Test/",
    stats.temp$model[1],
    "_0_",
    stats.temp$varivant[1],
    "_",
    stats.temp$Output.variable[1],
    ".txt"
  )
  write.csv("1",out.name)
  
  ## Dimension
  Noah=nc_open(stats.temp$path[1])
  all.vars = c(names(Noah$dim))
  if(c("lon")%in%all.vars){
    i.lon = ncvar_get(Noah,"lon")
    i.lat = ncvar_get(Noah,"lat")
  }
  if(c("longitude")%in%all.vars){
    i.lon = ncvar_get(Noah,"longitude")
    i.lat = ncvar_get(Noah,"latitude")
  }
  nc_close(Noah)
  
  ## Raw Data
  Time.series = data.frame("year" = rep(1000:2000,each = 12),
                           "month" = rep(1:12,times = length(c(1000:2000))))
  All_year = Time.series$year
  target_data = array(NA,c(length(i.lon),length(i.lat),length(All_year)))
  kk = 1
  
  ## Read Each File
  for(kk in 1:NROW(stats.temp)){
    Noah=nc_open(stats.temp$path[kk])
    i.data = ncvar_get(Noah,stats.temp$nc.variable[kk])
    nc_close(Noah)
    targe.years = c(stats.temp$startyear[kk]:stats.temp$endyear[kk])
    targe.years1 = rep(targe.years,each = 12)
    target_data[,,which(All_year%in%targe.years1)] = i.data[,,which(targe.years1%in%All_year)]
  }
  
  ## Sort Resolution
  i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
  ge.lon = order(i.lon,decreasing = FALSE)
  ge.lat = order(i.lat,decreasing = TRUE)
  i.lon = i.lon[ge.lon]
  i.lat = i.lat[ge.lat]
  lon.diff = diff(i.lon)[1]
  lat.diff = diff(i.lat)[1]
  target_data = target_data[ge.lon,ge.lat,]

  ## To Terra:Rast
  target_data = aperm(target_data,c(2,1,3))
  rm <- rast(target_data)
  ext(rm) <- c(-lon.diff, 360+lon.diff, -lat.diff-90, lat.diff+90)
  terra::time(rm) <- ymd(Time.series$year*10000 + Time.series$month*100 + 1)
  
  ## Re sample
  s <- rast(nrows=90, ncols=180, xmin=0, xmax=360, ymin=-90, ymax=90)
  t.total_resample = terra::resample(rm, s, method = "average")

  # ## Test
  # image.plot(target_data[,,1])
  # plot(rm[[1]])
  # plot(t.total_resample[[1]])
  
  ## Write data
  out.name = paste0(
    "Z:/2022/Science9_SM/Output Data/PMIIP_past1000/Gridded Data/",
    stats.temp$model[1],
    "_",
    stats.temp$varivant[1],
    "_",
    stats.temp$Output.variable[1],
    ".nc"
  )
  rr <-
    writeCDF(
      t.total_resample,
      out.name,
      overwrite = TRUE,
      varname = stats.temp$Output.variable[1],
      longname = "surface temperature",
      unit = "K"
    )
}

# Parallel ----------------------------------------------------------------

## Parallel 
library(parallel) # recon(1)
nge = c(1:38)
system.time({
  x <- nge
  cl <- makeCluster(3)
  results <- parLapply(cl, x, CMIP6_regrid)
  stopCluster(cl)
})

# End -------------------------------------------------------------


