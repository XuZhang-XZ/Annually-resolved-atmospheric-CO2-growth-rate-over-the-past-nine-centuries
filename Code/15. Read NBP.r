
# Paras -----------------------------------------------------------

## Data are collected from Trendy v12 dataset
## Several DGVMs that do not provide simulations at monthly timescales are not selected for analyses.
# [1] "CABLE-POP_S3_nbp.nc"    "CARDAMOM_S3_nbp.nc"     "CLASSIC_S3_nbp.nc"      "CLM5.0_S3_nbp.nc"       "DLEM_S3_nbp.nc"        
# [6] "E3SM_S3_nbp.nc"         "EDv3_S3_nbp.nc"         "IBIS_S3_nbp.nc"         "ISAM_S3_nbp.nc"         "ISBA-CTRIP_S3_nbp.nc"  
# [11] "JSBACH_S3_nbp.nc"       "JULES_S3_nbp.nc"        "LPJ-GUESS_S3_nbp.nc"    "LPJmL_S3_nbp.nc"        "LPJwsl_S3_nbp.nc"      
# [16] "LPX-Bern_S3_nbp.nc"     "OCN_S3_nbp.nc"          "ORCHIDEE_S3_nbp.nc"     "SDGVM_S3_nbp_Annual.nc" "VISIT_S3_nbp.nc"       
# [21] "YIBs_S3_Monthly_nbp.nc"

## DGVMs
DGVMs.models = list.files("Z:/2024/Input Data/Trendy_v12/S3_NBP/")
i = 3

## Interpolation
o.lon = seq(1,359,by = 2)
o.lat = seq(-89,89,by = 2)
All_time = ymd(19010101) + months(0:(121*12-1))
All_nbp = array(NA,c(21,length(o.lon),length(o.lat),length(All_time)))

# 1 ---------------------------------------------------------------

## Read NBP
i = 1
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit kg m-2 s-1 to PgC km-2 year-1
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte

# 2 NA ---------------------------------------------------------------

## This model do not provide monthly simulations and is not selected
i = 2
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
NA


# 3 ---------------------------------------------------------------

## Read NBP
i = 3
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17010101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 4 ---------------------------------------------------------------

## Read NBP
i = 4
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17010101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte

# 5 NA ---------------------------------------------------------------

## This model do not provide monthly simulations and is not selected
i = 5
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(17010101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
NA


# 6 ---------------------------------------------------------------

## Read NBP
i = 6
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
i.time/365 + 1
Tmp.time1= ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 7 ---------------------------------------------------------------

## Read NBP
i = 7
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 8 ---------------------------------------------------------------

## Read NBP
i = 8
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 9 ---------------------------------------------------------------

## Read NBP
i = 9
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"npp-rh+ld")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 10 ---------------------------------------------------------------

## Read NBP
i = 10
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon_FULL")
i.lat=ncvar_get(Noah, "lat_FULL")
i.time=ncvar_get(Noah, "time_counter")
Tmp.time1 = ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 11 ---------------------------------------------------------------

## Read NBP
i = 11
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 12 ---------------------------------------------------------------

## Read NBP
i = 12
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1= ymd(17000101) + months(1:length(i.time)) - months(1)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 13 NA ---------------------------------------------------------------

## This model do not provide monthly simulations and is not selected
i = 13
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = i.time + ymd(17000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
NA


# 14 NA ---------------------------------------------------------------

## This model do not provide monthly simulations and is not selected
i = 14
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
NA

# 15  ---------------------------------------------------------------

## Read NBP
i = 15
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 16  ---------------------------------------------------------------

## Read NBP
i = 16
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 17  ---------------------------------------------------------------

## Read NBP
i = 17
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal 1452
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 18  ---------------------------------------------------------------

## Read NBP
i = 18
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal 1452
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 19 NA  ---------------------------------------------------------------

## This model do not provide monthly simulations and is not selected
i = 19
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbpAnnual")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = ymd(17000101) + months(0:(length(i.time) - 1))
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
NA

# 20  ---------------------------------------------------------------

## Read NBP
i = 20
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = months(i.time-0.5) + ymd(18600101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# 21  ---------------------------------------------------------------

## Read NBP
i = 21
read.names = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",DGVMs.models[i])
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = months(i.time) + ymd(17000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Change Resolution
i.lon[which(i.lon<0)] = i.lon[which(i.lon<0)]+360
ge.lon = order(i.lon,decreasing = FALSE)
ge.lat = order(i.lat,decreasing = FALSE)
i.lon = i.lon[ge.lon]
i.lat = i.lat[ge.lat]
Tmp.data = Tmp.data[ge.lon,ge.lat,]

## Unit 
conver.unit = (10^6) * (1 / (10^12)) * 31536000
Tmp.data = Tmp.data * conver.unit

## Change Temporal
Tmp.data = Tmp.data[,,which(year(Tmp.time1) %in% c(1901:2021))]
dim(Tmp.data)

## Rigrid
tos_inte = conser(x = i.lon,y = i.lat, z = Tmp.data, xo = o.lon, yo = o.lat)
All_nbp[i,,,] = tos_inte


# Saving ----------------------------------------------------------

## Saving All Three Ensemble
Gess <- ncdim_def(name = "DGVMs", units = "nbp", vals = 1:21)
x <- ncdim_def(name = "lon", units = "degrees_east", vals = o.lon)
y <- ncdim_def(name = "lat", units = "degrees_north", vals = o.lat)
t <- ncdim_def(name = "time", units = "days since 1700-01-01",vals = as.numeric(All_time - ymd(17000101)), unlim = TRUE)
var1 <- ncvar_def("nbp", "pgC years", list(Gess, x, y, t), NA, prec = "float")
vars <- list(var1)
ncnew <- nc_create(paste0("Z:/2022/Science9_SM/Output Data/DGVM/All_nbp.nc"), vars)
ncvar_put(ncnew, var1, All_nbp)
ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
nc_close(ncnew)

## Remove DGVMs that do not provide simulations at monthly timescales 
Sel.model = c(1:21)[-c(2,5,13,14,19)]
All_nbp_Sel = All_nbp[Sel.model,,,]

## Saving All Three Ensemble
Gess <- ncdim_def(name = "DGVMs", units = "nbp", vals = 1:length(Sel.model))
x <- ncdim_def(name = "lon", units = "degrees_east", vals = o.lon)
y <- ncdim_def(name = "lat", units = "degrees_north", vals = o.lat)
t <- ncdim_def(name = "time", units = "days since 1700-01-01",vals = as.numeric(All_time - ymd(17000101)), unlim = TRUE)
var1 <- ncvar_def("nbp", "pgC years", list(Gess, x, y, t), NA, prec = "float")
vars <- list(var1)
ncnew <- nc_create(paste0("Z:/2022/Science9_SM/Output Data/DGVM/All_nbp_series.nc"), vars)
ncvar_put(ncnew, var1, All_nbp_Sel)
ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
nc_close(ncnew)

# Global Time Series -------------------------------------------------------------

## Read NBP
read.names = "Z:/2022/Science9_SM/Output Data/DGVM/All_nbp_series.nc"
Noah=nc_open(read.names)
Tmp.data=ncvar_get(Noah,"nbp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1 = days(i.time) + ymd(17000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Read Data
NBP_Files = list.files("Z:/2024/Input Data/Trendy_v12/S3_NBP/")

## Select
Sel.model = c(1:21)[-c(2,5,13,14,19)]
NBP_Files = NBP_Files[Sel.model]

## Area Weight
A.Weight = array(rep(cos(i.lat * pi / 180), each = length(i.lon)), c(length(i.lon), length(i.lat))) * 111 * 2 * 111 * 2

## Data
NBP_Data = data.frame()
i = 5

## Read
for(i in 1:length(NBP_Files)) {
  
  ## Time Series
  NBP_1 = data.frame(
    "year" = year(Tmp.time1),
    "month" = month(Tmp.time1),
    "DGVM" = NBP_Files[i],
    "NBP" = areal.mean_sum(z = Tmp.data[i,,,], A.Weight = A.Weight)
  )
  
  ## Combine
  NBP_Data = rbind(NBP_Data,NBP_1)

}

## Summary
NBP_Data_A_T = NBP_Data %>%
  dplyr::group_by(DGVM, year) %>%
  dplyr::summarise(NBP = mean(NBP)) %>%
  dplyr::group_by(DGVM) %>%
  dplyr::reframe(year = year,
                 NBP = pass.filter.NA.mean(
                   a = NBP,
                   W = 10,
                   type = "high",
                   method = c("Butterworth")
                 ))
NBP_Data_A = NBP_Data_A_T %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(NBP = median(NBP)) %>%
  dplyr::mutate(NBP = NBP / 2.124)

## Write
write.csv(NBP_Data_A,"Output Data/Reconstructed CGR/SD/NBP_Data_A.csv")

## Plot
p1 = ggplot() +
  geom_path(data = NBP_Data_A_T,
            aes(x = year, y = NBP,colour = DGVM))

# Simple Validation -----------------------------------------------------------------

## Data
Total.summary = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
NBP_Data_A = read.csv("Output Data/Reconstructed CGR/SD/NBP_Data_A.csv", row.names = 1)

## Total
Total = merge(NBP_Data_A, Total.summary, all.x = TRUE)

## Total
Total1 = Total[Total$year %in% c(1915:1958), ]
cor(Total1$NBP, Total1$Pre.median)
Total1 = Total[Total$year %in% c(1959:2000), ]
cor(Total1$NBP, Total1$Pre.median)

## Plot
plot(Total.summary$year,scale(Total.summary$Pre.median),type = "l",xlim = c(1900,2000))
lines(NBP_Data_A$year,-scale(NBP_Data_A$NBP), col = "red")
abline(v = c(1991,1992,1993))

## Plot
plot(Total.summary$year,scale(Total.summary$Total.Obs),type = "l",xlim = c(1980,2000))
lines(NBP_Data_A$year,-scale(NBP_Data_A$NBP), col = "red")
abline(v = c(1991,1992))

## Plot
plot(Total.summary$year,Total.summary$Pre.median,type = "l",xlim = c(1800,2000))
lines(NBP_Data_A$year,-NBP_Data_A$NBP, col = "red")

# End -------------------------------------------------------------












# 
# 
# ## Read Data
# NBP_Files = list.files("Z:/2024/Input Data/Trendy_v12/S3_NBP/")
# NBP_Data = data.frame()
# i = 5
# 
# ## Remove
# NBP_Files = NBP_Files[which(!NBP_Files %in% c("CARDAMOM_S3_nbp.nc"))]
# 
# ## Read
# for(i in 1:length(NBP_Files)) {
#   
#   ## Read Data
#   if(NBP_Files[i] == "CABLE-POP_S3_nbp.nc") {
#     Read_name = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",NBP_Files[i])
#     Noah=nc_open(Read_name)
#     PDSI.data=ncvar_get(Noah,"nbp")
#     i.lon=ncvar_get(Noah, "longitude")
#     i.lat=ncvar_get(Noah, "latitude")
#     i.time=ncvar_get(Noah, "time")
#     i.time1=month(i.time)+ymd(17000115)
#     nc_close(Noah)
#   }
#   if(NBP_Files[i] == "CLASSIC_S3_nbp.nc") {
#     Read_name = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",NBP_Files[i])
#     Noah=nc_open(Read_name)
#     PDSI.data=ncvar_get(Noah,"nbp")
#     i.lon=ncvar_get(Noah, "longitude")
#     i.lat=ncvar_get(Noah, "latitude")
#     i.time=ncvar_get(Noah, "time")
#     i.time1=day(i.time)+ymd(17001231)
#     nc_close(Noah)
#   }
#   if(NBP_Files[i] == "CLM5.0_S3_nbp.nc") {
#     Read_name = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",NBP_Files[i])
#     Noah=nc_open(Read_name)
#     PDSI.data=ncvar_get(Noah,"nbp")
#     i.lon=ncvar_get(Noah, "lon")
#     i.lat=ncvar_get(Noah, "lat")
#     i.time=ncvar_get(Noah, "time")
#     i.time1=days(i.time)+ymd(17000101)
#     nc_close(Noah)
#   }
#   if(NBP_Files[i] == "DLEM_S3_nbp.nc") {
#     Read_name = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",NBP_Files[i])
#     Noah=nc_open(Read_name)
#     PDSI.data=ncvar_get(Noah,"nbp")
#     i.lon=ncvar_get(Noah, "lon")
#     i.lat=ncvar_get(Noah, "lat")
#     i.time=ncvar_get(Noah, "time")
#     i.time1=days(i.time)+ymd(17010101)
#     nc_close(Noah)
#   }
#   if(NBP_Files[i] == "E3SM_S3_nbp.nc") {
#     Read_name = paste0("Z:/2024/Input Data/Trendy_v12/S3_NBP/",NBP_Files[i])
#     Noah=nc_open(Read_name)
#     PDSI.data=ncvar_get(Noah,"nbp")
#     i.lon=ncvar_get(Noah, "lon")
#     i.lat=ncvar_get(Noah, "lat")
#     i.time=ncvar_get(Noah, "time")
#     i.time1=days(round(i.time))+ymd(00010101)
#     nc_close(Noah)
#   }
#   
#   ## A Weight
#   A.Weight = array(rep(cos(i.lat * pi / 180), each = length(i.lon)), c(length(i.lon), length(i.lat)))
#   NBP_1 = data.frame(
#     "year" = year(i.time1),
#     "DGVM" = NBP_Files[i],
#     "NBP" = areal.mean(z = PDSI.data, A.Weight = A.Weight)
#   )
#   
#   ## Combine
#   NBP_Data = rbind(NBP_Data,NBP_1)
#   
#   
# }
