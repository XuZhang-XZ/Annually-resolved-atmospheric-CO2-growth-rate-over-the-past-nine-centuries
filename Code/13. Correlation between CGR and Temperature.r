

# Cor with HadCRUT Temperature ------------------------------------------------

## CGR
CGR=read.csv("Output Data/CGR/CGR.csv")

## Global Temperature
Noah=nc_open("Z:/2023/Input data/HadCRUT5/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc")
Tmp.data=ncvar_get(Noah,"tas_mean")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(18500101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
nc_close(Noah)

## Select Years
Tmp.time1[length(Tmp.time1)]
Tmp.time1 = Tmp.time1[1:(12*171)]
Tmp.data = Tmp.data[,,c(1:(12*171))]
Tmp.year = c(1850:(1850+171-1))

## Three Periods
A.periods = c("Annual","Summer","Winter")
Cor.data = data.frame()
i.period = 1

## Correlation
for(i.period in 1:3){
  
  ## Annual, Summer Winter Three periods
  if(A.periods[i.period] == "Annual"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 1)]
    for(i in 2:12){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/12
  }
  if(A.periods[i.period] == "Summer"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 6)]
    for(i in 7:8){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/3
  }
  if(A.periods[i.period] == "Winter"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 12)]
    A.Tmp1 = Tmp.data[,,which(month(Tmp.time1) == 12)]
    A.Tmp[,,1] = NA
    A.Tmp[,,c(2:length(Tmp.year))] = A.Tmp1[,,c(1:c(length(Tmp.year)-1))]
    for(i in 1:2){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/3
  }
  
  ## 10-year high pass filter
  A.Tmp = apply(A.Tmp,c(1,2),pass.filter.NA.mean, W=10)
  A.Tmp = aperm(A.Tmp, c(2, 3, 1))
  Cor.data1 = expand.grid(Lon = i.lon, Lat = i.lat)
  Cor.data1$Period = A.periods[i.period]
  
  ## Current Year
  Target.year = Tmp.year
  Tmp.Cor = apply(A.Tmp[,,which(Target.year%in%c(1959:2020))],c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1959:2020))])
  Cor.data1[,c("A.0")] = as.numeric(Tmp.Cor[1,,])
  Cor.data1[,c("A.p.0")] = as.numeric(Tmp.Cor[2,,])
  
  ## Preceding Year
  Target.year = Tmp.year+1
  Tmp.Cor = apply(A.Tmp[,,which(Target.year%in%c(1959:2020))],c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1959:2020))])
  Cor.data1[,c("A.1")] = as.numeric(Tmp.Cor[1,,])
  Cor.data1[,c("A.p.1")] = as.numeric(Tmp.Cor[2,,])
  
  ## Combine
  Cor.data = rbind(Cor.data ,Cor.data1)
  
  ## Print
  print(i.period)
}

## Testing
if(1 == 0) {
  image.plot(Tmp.data[,,1350])
  image.plot(A.Tmp[,,100])
}

## Data
Noah=nc_open("Z:/2023/Input data/HadCRUT5/HadCRUT.5.0.1.0.analysis.weights.nc")
Land.c=ncvar_get(Noah,"weights")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
nc_close(Noah)

## Land Coverage
Land.coverage = expand.grid(Lon = i.lon, Lat = i.lat) %>%
  dplyr::mutate(Weight = as.numeric(Land.c[, , 2000])) %>%
  dplyr::filter(Weight < 0.8) %>%
  dplyr::mutate(ID = paste0(Lon, "_", Lat))

## Select
Cor.data_Sel = Cor.data %>%
  dplyr::mutate(ID = paste0(Lon, "_", Lat))%>%
  dplyr::filter(ID%in%Land.coverage$ID) %>%
  dplyr::select(!ID)

## Write
write.csv(Cor.data_Sel,"Output Data/CGR/Cor with HadCRUT.csv", row.names = F)

# Cor with CRU T ------------------------------------------------------

## CGR
CGR=read.csv("Output Data/CGR/CGR.csv")

## Global Temperature
Noah=nc_open("Z:/2023/Input data/CRU TS v4.07/cru_ts4.07.1901.2022.tmp.dat.nc")
Tmp.data=ncvar_get(Noah,"tmp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(19000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
Tmp.year = unique(year(Tmp.time1))
nc_close(Noah)

## Three Periods
A.periods = c("Annual","Summer","Winter")
Cor.data = data.frame()
i.period = 3

## Correlation
for(i.period in 1:3){
  
  ## Annual or Summer Values
  if(A.periods[i.period] == "Annual"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 1)]
    for(i in 2:12){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/12
  }
  if(A.periods[i.period] == "Summer"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 6)]
    for(i in 7:8){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/3
  }
  if(A.periods[i.period] == "Winter"){
    A.Tmp = Tmp.data[,,which(month(Tmp.time1) == 12)]
    A.Tmp1 = Tmp.data[,,which(month(Tmp.time1) == 12)]
    A.Tmp[,,1] = NA
    A.Tmp[,,c(2:length(Tmp.year))] = A.Tmp1[,,c(1:c(length(Tmp.year)-1))]
    for(i in 1:2){
      A.Tmp = A.Tmp+Tmp.data[,,which(month(Tmp.time1) == i)]
    }
    A.Tmp = A.Tmp/3
  }
  
  ## 10-year high pass filter
  A.Tmp = apply(A.Tmp,c(1,2),pass.filter.NA.mean, W=10)
  A.Tmp = aperm(A.Tmp, c(2, 3, 1))
  Cor.data1 = expand.grid(Lon = i.lon, Lat = i.lat)
  Cor.data1$Period = A.periods[i.period]
  
  ## Current Year
  Target.year = Tmp.year
  Tmp.Cor = apply(A.Tmp[,,which(Target.year%in%c(1959:2020))],c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1959:2020))])
  Cor.data1[,c("A.0")] = as.numeric(Tmp.Cor[1,,])
  Cor.data1[,c("A.p.0")] = as.numeric(Tmp.Cor[2,,])
  
  ## Preceding Year
  Target.year = Tmp.year+1
  Tmp.Cor = apply(A.Tmp[,,which(Target.year%in%c(1959:2020))],c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1959:2020))])
  Cor.data1[,c("A.1")] = as.numeric(Tmp.Cor[1,,])
  Cor.data1[,c("A.p.1")] = as.numeric(Tmp.Cor[2,,])
  
  ## Combine
  Cor.data = rbind(Cor.data ,Cor.data1)
  
  ## Print
  print(i.period)
}

## Write
Cor.data_Sel = Cor.data
write.csv(Cor.data_Sel,"Output Data/CGR/Cor with CRU_Tmp.csv", row.names = F)

## Plot
colors8=brewer.pal(n=11,"Spectral")[11:1]
p1 <-   ggplot() +
  geom_tile(
    data = Cor.data_Sel%>%dplyr::filter(A.p.0<0.05),color = NA,
    aes(x = Lon, y =Lat, fill = A.0)
  ) +
  geom_polygon(data=con.boundary1,
               color = "gray10", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_hline(yintercept = c(-30,30), col = "blue", linetype = "dashed")+
  scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
  coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0))
p2 <-   ggplot() +
  geom_tile(
    data = Cor.data_Sel%>%dplyr::filter(A.p.1<0.05),color = NA,
    aes(x = Lon, y =Lat, fill = A.1)
  ) +
  geom_polygon(data=con.boundary1,
               color = "gray10", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_hline(yintercept = c(-30,30), col = "blue", linetype = "dashed")+
  scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
  coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0))


# End -------------------------------------------------------------

# 
# # Cor with CRU P
# 
# ## CGR
# CGR=read.csv("Output Data/CGR/CGR.csv")
# 
# ## Global Temperature
# Noah=nc_open("E:/2022-20220617/Input data/CRU TS v. 4.05/cru_ts4.05.1901.2020.pre.dat.nc")
# Tmp.data=ncvar_get(Noah,"pre")
# i.lon=ncvar_get(Noah, "lon")
# i.lat=ncvar_get(Noah, "lat")
# i.time=ncvar_get(Noah, "time")
# Tmp.time1=i.time+ymd(19000101)
# Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
# Tmp.year = unique(year(Tmp.time1))
# nc_close(Noah)
# 
# ## Read Data
# ch =1; ge.mon =1
# Cor.data = expand.grid(Lon = i.lon, Lat = i.lat)
# for(ch in c(0,1)){
#   Tmp.year1 = Tmp.year+ch
#   for(ge.mon in c(1,4,7,10)){
#     
#     ges1 = length(c(1959:2020))
#     Tmp.data1 = Tmp.data[,,which(month(Tmp.time1) == ge.mon)]
#     Tmp.data2 = Tmp.data[,,which(month(Tmp.time1) == c(1+ge.mon))]
#     Tmp.data3 = Tmp.data[,,which(month(Tmp.time1) == c(2+ge.mon))]
#     Tmp.data.T = (Tmp.data1+Tmp.data2+Tmp.data1)/3
#     Tmp.data.T = Tmp.data.T[,,which(Tmp.year1%in%c(1960:2015))]
#     
#     Tmp.data.T = apply(Tmp.data.T,c(1,2),pass.filter.NA.mean, W=10)
#     Tmp.data.T = aperm(Tmp.data.T, c(2, 3, 1))
#     
#     Tmp.Cor = apply(Tmp.data.T,c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1960:2015))])
#     
#     Cor.data[,paste0("Cor.",ch,".",ge.mon)] = as.numeric(Tmp.Cor[1,,])
#     Cor.data[,paste0("p.",ch,".",ge.mon)] = as.numeric(Tmp.Cor[2,,])
#     print(ge.mon)
#   }
# }
# 
# ## Plot
# plot.cols = c(paste0("Cor.0.",c(1,4,7,10)),paste0("Cor.1.",c(1,4,7,10)))
# plot.cols.p = c(paste0("p.0.",c(1,4,7,10)),paste0("p.1.",c(1,4,7,10)))
# colors8=brewer.pal(n=11,"Spectral")[11:1]
# pp = paste0("Cor.T_",c(1:8))
# i = 5
# for(i in 1:8){
#   assign(pp[i],
#          world_map1 <-   ggplot() +
#            geom_tile(
#              data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],color = NA,
#              aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#            ) +
#            geom_polygon(data=con.boundary1,
#                         color = "gray10", fill = NA,size=0.1,
#                         aes(x = long, y = lat, group = group)) +
#            scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
#            coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0)) +
#            P_Xu_F1 +
#            theme(
#              plot.margin = margin(3, 3, 3, 3, "pt"),
#              axis.title = element_blank(),
#              legend.key.width = unit(60, "pt"),
#              legend.key.height = unit(7, "pt"),
#              legend.position = "none"
#            )
#   )
# }
# 
# ## Legend
# i = 1
# legend1 <-   ggplot() +
#   geom_tile(
#     data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],
#     size = 2,
#     aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#   ) +
#   scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=11)+
#   theme(
#     plot.margin = margin(3, 3, 3, 3, "pt"),
#     legend.position =c(0.5,0.5),
#     legend.direction = "horizontal",
#     legend.key.width = unit(70, "pt"),
#     legend.key.height = unit(7, "pt"),
#     legend.title = element_blank(),
#   )
# legend1 <- get_legend(legend1)
# 
# # Saving
# plot.map=plot_grid(Cor.T_1, Cor.T_2, Cor.T_3, Cor.T_4,
#                    Cor.T_5,Cor.T_6,Cor.T_7,Cor.T_8,
#                    nrow=4,labels = "auto")
# plot.total = plot_grid(plot.map,legend1,nrow = 2,rel_heights = c(1,0.05))
# ggsave(plot = plot.total,
#        filename = "Figures/Performance/Cor with Pre_CRU.jpeg",
#        # device = cairo_pdf,
#        dpi=450,
#        width=8.6,
#        height=9)
# 
# 
# # Cor with CRU PDSI 
# 
# ## CGR
# CGR=read.csv("Output Data/CGR/CGR.csv")
# 
# ## Global Temperature
# Noah=nc_open("E:/2022-20220617/Input data/CRU TS v. 4.05/scPDSI.cru_ts4.05early1.1901.2020.cal_1901_20.bams.2021.GLOBAL.1901.2020.nc")
# Tmp.data=ncvar_get(Noah,"scpdsi")
# Tmp.data = Tmp.data[,c(360:1),]
# i.lon=ncvar_get(Noah, "longitude")
# i.lat=ncvar_get(Noah, "latitude")[360:1]
# i.time=ncvar_get(Noah, "time")
# i.time1=i.time+ymd(19000101)
# Tmp.year = unique(year(Tmp.time1))
# nc_close(Noah)
# 
# ## Read Data
# ch =1; ge.mon =1
# Cor.data = expand.grid(Lon = i.lon, Lat = i.lat)
# for(ch in c(0,1)){
#   Tmp.year1 = Tmp.year+ch
#   for(ge.mon in c(1,4,7,10)){
#     
#     ges1 = length(c(1959:2020))
#     Tmp.data1 = Tmp.data[,,which(month(Tmp.time1) == ge.mon)]
#     Tmp.data2 = Tmp.data[,,which(month(Tmp.time1) == c(1+ge.mon))]
#     Tmp.data3 = Tmp.data[,,which(month(Tmp.time1) == c(2+ge.mon))]
#     Tmp.data.T = (Tmp.data1+Tmp.data2+Tmp.data1)/3
#     Tmp.data.T = Tmp.data.T[,,which(Tmp.year1%in%c(1960:2015))]
#     
#     Tmp.data.T = apply(Tmp.data.T,c(1,2),pass.filter.NA.mean, W=10)
#     Tmp.data.T = aperm(Tmp.data.T, c(2, 3, 1))
#     
#     Tmp.Cor = apply(Tmp.data.T,c(1,2),Cor.CGR,x2 = CGR$Total.Obs[which(CGR$year%in%c(1960:2015))])
#     
#     Cor.data[,paste0("Cor.",ch,".",ge.mon)] = as.numeric(Tmp.Cor[1,,])
#     Cor.data[,paste0("p.",ch,".",ge.mon)] = as.numeric(Tmp.Cor[2,,])
#     print(ge.mon)
#   }
# }
# 
# ## Plot
# plot.cols = c(paste0("Cor.0.",c(1,4,7,10)),paste0("Cor.1.",c(1,4,7,10)))
# plot.cols.p = c(paste0("p.0.",c(1,4,7,10)),paste0("p.1.",c(1,4,7,10)))
# colors8=brewer.pal(n=11,"Spectral")[11:1]
# pp = paste0("Cor.T_",c(1:8))
# i = 5
# for(i in 1:8){
#   assign(pp[i],
#          world_map1 <-   ggplot() +
#            geom_tile(
#              data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],color = NA,
#              aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#            ) +
#            geom_polygon(data=con.boundary1,
#                         color = "gray10", fill = NA,size=0.1,
#                         aes(x = long, y = lat, group = group)) +
#            scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
#            coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0)) +
#            P_Xu_F1 +
#            theme(
#              plot.margin = margin(3, 3, 3, 3, "pt"),
#              axis.title = element_blank(),
#              legend.key.width = unit(60, "pt"),
#              legend.key.height = unit(7, "pt"),
#              legend.position = "none"
#            )
#   )
# }
# 
# ## Legend
# i = 1
# legend1 <-   ggplot() +
#   geom_tile(
#     data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],
#     size = 2,
#     aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#   ) +
#   scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=11)+
#   theme(
#     plot.margin = margin(3, 3, 3, 3, "pt"),
#     legend.position =c(0.5,0.5),
#     legend.direction = "horizontal",
#     legend.key.width = unit(70, "pt"),
#     legend.key.height = unit(7, "pt"),
#     legend.title = element_blank(),
#   )
# legend1 <- get_legend(legend1)
# 
# # Saving
# plot.map=plot_grid(Cor.T_1, Cor.T_2, Cor.T_3, Cor.T_4,
#                    Cor.T_5,Cor.T_6,Cor.T_7,Cor.T_8,
#                    nrow=4,labels = "auto")
# plot.total = plot_grid(plot.map,legend1,nrow = 2,rel_heights = c(1,0.05))
# ggsave(plot = plot.total,
#        filename = "Figures/Performance/Cor with PDSI_CRU.jpeg",
#        # device = cairo_pdf,
#        dpi=450,
#        width=8.6,
#        height=9)

# ## Legend
# i = 1
# legend1 <-   ggplot() +
#   geom_tile(
#     data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],
#     size = 2,
#     aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#   ) +
#   scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=11)+
#   theme(
#     plot.margin = margin(3, 3, 3, 3, "pt"),
#     legend.position =c(0.5,0.5),
#     legend.direction = "horizontal",
#     legend.key.width = unit(70, "pt"),
#     legend.key.height = unit(7, "pt"),
#     legend.title = element_blank(),
#   )
# legend1 <- get_legend(legend1)
# 
# # Saving
# plot.map=plot_grid(Cor.T_1, Cor.T_2, Cor.T_3, Cor.T_4,
#                    Cor.T_5,Cor.T_6,Cor.T_7,Cor.T_8,
#                    nrow=4,labels = "auto")
# plot.total = plot_grid(plot.map,legend1,nrow = 2,rel_heights = c(1,0.05))
# ggsave(plot = plot.total,
#        filename = "Figures/Performance/Cor with Tem_CRU.jpeg",
#        # device = cairo_pdf,
#        dpi=450,
#        width=8.6,
#        height=9)


# Land.coverage$Weight = as.numeric(Land.c[,,2000])
# Land.coverage = Land.coverage%>%dplyr::filter(Weight<0.8)
# Land.coverage$ID = paste0(Land.coverage$Lon,"_",Land.coverage$Lat)











# 
# i = 300
# j = 100
# Cor.data = data.frame()
# for(i in 1:720){
#   for(j in 1:360){
#     T.data =data.frame("Date" = Tmp.time1,
#                        "Year" = year(Tmp.time1),
#                        "month" = month(Tmp.time1),
#                        "Tem" = Tmp.data[i,j,])
#     T.data = T.data[which(T.data$Year>=1959),]
#     if(length(which(complete.cases(T.data)))<12*50){
#       next
#     }
#     Cor.data1 = data.frame()
#     Cor.data1[1,c("Lon","Lat")] = c(i.lon[i],i.lat[j])
#     
#     ## Year 
#     ch =1; ge.mon =3
#     for(ch in c(0,1)){
#       T.data1 = T.data
#       T.data1$Year = T.data1$Year+ch
#       for(ge.mon in c(1,4,7,10)){
#         T.data2 = T.data1[which(T.data1$month%in%c(ge.mon:(ge.mon+2))),]
#         A.T.data1 = aggregate(Tem~Year, data =T.data2, mean)
#         names(A.T.data1) = c("year","A.T")
#         A.T.data1$A.T = pass.filter.NA.mean(A.T.data1$A.T,W = 10, type="high")
#         Total = merge(A.T.data1,CGR)
#         temp1 = cor.test(Total$A.T, Total$Obs)
#         
#         Cor.data1[1,paste0("Cor.",ch,".",ge.mon)] = temp1$estimate
#         Cor.data1[1,paste0("p.",ch,".",ge.mon)] = temp1$p.value
#       }
#     }
#     Cor.data = rbind(Cor.data,Cor.data1)
#   }
#   print(i)
# }
# 
# ## Plot
# plot.cols = c(paste0("Cor.0.",c(1,4,7,10)),paste0("Cor.1.",c(1,4,7,10)))
# plot.cols.p = c(paste0("p.0.",c(1,4,7,10)),paste0("p.1.",c(1,4,7,10)))
# colors8=brewer.pal(n=11,"Spectral")[11:1]
# pp = paste0("Cor.T_",c(1:8))
# i = 1
# for(i in 1:8){
#   assign(pp[i],
#          world_map1 <-   ggplot() +
#            geom_tile(
#              data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],color = NA,
#              aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#            ) +
#            geom_polygon(data=con.boundary1,
#                         color = "gray10", fill = NA,size=0.1,
#                         aes(x = long, y = lat, group = group)) +
#            scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
#            coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0)) +
#            P_Xu_F1 +
#            theme(
#              plot.margin = margin(3, 3, 3, 3, "pt"),
#              axis.title = element_blank(),
#              legend.key.width = unit(60, "pt"),
#              legend.key.height = unit(7, "pt"),
#              legend.position = "none"
#            )
#   )
# }
# 
# ## Legend
# i = 1
# legend1 <-   ggplot() +
#   geom_tile(
#     data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],
#     size = 2,
#     aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#   ) +
#   scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=11)+
#   theme(
#     plot.margin = margin(3, 3, 3, 3, "pt"),
#     legend.position =c(0.5,0.5),
#     legend.direction = "horizontal",
#     legend.key.width = unit(70, "pt"),
#     legend.key.height = unit(7, "pt"),
#     legend.title = element_blank(),
#   )
# legend1 <- get_legend(legend1)
# 
# # Saving
# plot.map=plot_grid(Cor.T_1, Cor.T_2, Cor.T_3, Cor.T_4,
#                    Cor.T_5,Cor.T_6,Cor.T_7,Cor.T_8,
#                    nrow=4,labels = "auto")
# plot.total = plot_grid(plot.map,legend1,nrow = 2,rel_heights = c(1,0.05))
# ggsave(plot = plot.total,
#        filename = "Figures/Performance/Cor with Tem_CRU.jpeg",
#        # device = cairo_pdf,
#        dpi=450,
#        width=8.6,
#        height=9)








# 
# ## Plot
# plot.cols = c("Cor.Annual","Cor.10","Cor.7","Cor.4","Cor.1")
# plot.cols.p = c("Cor.p.Annual","Cor.p.10","Cor.p.7","Cor.p.4","Cor.p.1")
# colors8=brewer.pal(n=11,"Spectral")[11:1]
# pp = paste0("Cor.T_",c(1:5))
# i = 1
# for(i in 1:5){
#   assign(pp[i],
#          world_map1 <-   ggplot() +
#            geom_tile(
#              data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],color = NA,
#              aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#            ) +
#            geom_polygon(data=con.boundary1,
#                         color = "gray10", fill = NA,size=0.1,
#                         aes(x = long, y = lat, group = group)) +
#            scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
#            coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0)) +
#            P_Xu_F1 +
#            theme(
#              plot.margin = margin(3, 3, 3, 3, "pt"),
#              axis.title = element_blank(),
#              legend.key.width = unit(60, "pt"),
#              legend.key.height = unit(7, "pt"),
#              legend.position = "none"
#            )
#   )
# }
# 
# ## Legend
# i = 1
# legend1 <-   ggplot() +
#   geom_tile(
#     data = Cor.data[which(Cor.data[,plot.cols.p[i]]<0.1),],
#     size = 2,
#     aes_string(x = "Lon", y ="Lat", fill = plot.cols[i])
#   ) +
#   scale_fill_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=11)+
#   theme(
#     plot.margin = margin(3, 3, 3, 3, "pt"),
#     legend.position =c(0.5,0.5),
#     legend.direction = "horizontal",
#     legend.key.width = unit(70, "pt"),
#     legend.key.height = unit(7, "pt"),
#     legend.title = element_blank(),
#   )
# legend1 <- get_legend(legend1)
# 
# # Saving
# plot.map=plot_grid(Cor.T_1, Cor.T_2, Cor.T_3, Cor.T_4, Cor.T_5,nrow=3,rel_heights = c(1,1,1),labels = "auto")
# plot.total = plot_grid(plot.map,legend1,nrow = 2,rel_heights = c(1,0.1))
# ggsave(plot = plot.total,
#        filename = "Figures/Performance/Cor with Tem.pdf",
#        device = cairo_pdf,
#        dpi=450,
#        width=8.6,
#        height=7)




# 
# ## Read Data
# 
# i = 600
# j = 300
# Cor.data = data.frame()
# for(i in 1:720){
#   for(j in 1:360){
#     
#     ## Data
#     T.data =data.frame("Date" = Tmp.time1,
#                        "Year" = year(Tmp.time1),
#                        "month" = month(Tmp.time1),
#                        "Tem" = A.Tmp[i,j,])
#     # T.data = T.data[which(T.data$month%in%c(1:5)),]
#     # JJA.data = T.data[which(T.data$month%in%c(1:5)),]
#     
#     ## Initital Data
#     T.data1 = T.data[which(T.data$Year>=1959),]
#     JJA.data1 = JJA.data[which(JJA.data$Year>=1959),]
#     Cor.data1 = data.frame()
#     Cor.data1[1,c("Lon","Lat")] = c(i.lon[i],i.lat[j])
#     
#     ## Test Series
#     if(length(which(complete.cases(T.data1)))<(NROW(T.data1)*0.8)|
#        length(which(complete.cases(JJA.data1)))<(NROW(JJA.data1)*0.8)){
#       next
#     }
#     
#     ## Annual Mean
#     A.T.data = aggregate(Tem~Year, data =T.data, mean)
#     names(A.T.data) = c("year","Annua.T")
#     A.T.data$Annua.T.10=pass.filter.NA(a=A.T.data$Annua.T,W=10,type="high",method = c("Butterworth"))
#     
#     ## Correlation in current year
#     Total.data = merge(A.T.data,CGR)
#     Cor.data1[1,c("A.0","A.p.0")] = cor30.1(Total.data$Annua.T.10,Total.data$Total.Obs1)
#     
#     ## Correlation in preceding year
#     A.T.data1= A.T.data
#     A.T.data1$year = A.T.data1$year+1
#     Total.data = merge(A.T.data1,CGR)
#     Cor.data1[1,c("A.1","A.p.1")] = cor30.1(Total.data$Annua.T.10,Total.data$Total.Obs1)
#     
#     ## Total
#     Cor.data = rbind(Cor.data,Cor.data1)
#     
#   }
#   print(i)
# }
# 
# ## Annual
# A.T.data = aggregate(Tem~Year, data =T.data, mean)
# names(A.T.data) = c("year","Annua.T")
# 
# T.data1 = T.data[which(T.data$month%in%c(10:12,1:9)),]
# T.data1$Year[which(T.data1$month%in%c(10:12))] = 1 + T.data1$Year[which(T.data1$month%in%c(10:12))]
# A.T.data1 = aggregate(Tem~Year, data =T.data1, mean)
# names(A.T.data1) = c("year","T.10")
# 
# T.data1 = T.data[which(T.data$month%in%c(7:12,1:6)),]
# T.data1$Year[which(T.data1$month%in%c(7:12))] = 1 + T.data1$Year[which(T.data1$month%in%c(7:12))]
# A.T.data2 = aggregate(Tem~Year, data =T.data1, mean)
# names(A.T.data2) = c("year","T.7")
# 
# T.data1 = T.data[which(T.data$month%in%c(4:12,1:3)),]
# T.data1$Year[which(T.data1$month%in%c(4:12))] = 1 + T.data1$Year[which(T.data1$month%in%c(4:12))]
# A.T.data3 = aggregate(Tem~Year, data =T.data1, mean)
# names(A.T.data3) = c("year","T.4")
# 
# T.data1 = T.data[which(T.data$month%in%c(1:12)),]
# T.data1$Year[which(T.data1$month%in%c(1:12))] = 1 + T.data1$Year[which(T.data1$month%in%c(1:12))]
# A.T.data4 = aggregate(Tem~Year, data =T.data1, mean)
# names(A.T.data4) = c("year","T.1")
# 
# ## Total
# Total = merge(A.T.data,A.T.data1)
# Total = merge(Total,A.T.data2)
# Total = merge(Total,A.T.data3)
# Total = merge(Total,A.T.data4)
# Total[,-1] = apply(Total[,-1], 2, pass.filter.NA.mean, W=10)
# Total = merge(Total,CGR)
# if(NROW(Total)<30){
#   next
# }
# 
# ## Correlation
# Cor.data1 = data.frame()
# Cor.data1[1,c("Lon","Lat")] = c(i.lon[i],i.lat[j])
# temp1 = cor.test(Total$Annua.T, Total$Obs)
# temp2 = cor.test(Total$T.10, Total$Obs)
# temp3 = cor.test(Total$T.7, Total$Obs)
# temp4 = cor.test(Total$T.4, Total$Obs)
# temp5 = cor.test(Total$T.1, Total$Obs)
# Cor.data1[1,c("Cor.Annual","Cor.10","Cor.7","Cor.4","Cor.1")] = c(temp1$estimate,temp2$estimate,temp3$estimate,temp4$estimate,temp5$estimate)
# Cor.data1[1,c("Cor.p.Annual","Cor.p.10","Cor.p.7","Cor.p.4","Cor.p.1")] = c(temp1$p.value,temp2$p.value,temp3$p.value,temp4$p.value,temp5$p.value)
# Cor.data = rbind(Cor.data,Cor.data1)
# 
# 
# 
# ## CGR
# CGR=read.csv("Output Data/CGR/CGR.csv")
# 
# ## Global Temperature
# Noah=nc_open("E:/2022-20220617/Input data/CRU TS v. 4.05/cru_ts4.05.1901.2020.tmp.dat.nc")
# Tmp.data=ncvar_get(Noah,"tmp")
# i.lon=ncvar_get(Noah, "lon")
# i.lat=ncvar_get(Noah, "lat")
# i.time=ncvar_get(Noah, "time")
# Tmp.time1=i.time+ymd(19000101)
# Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
# nc_close(Noah)
# 
# ## Read Data
# i = 30
# j = 10
# Cor.data = data.frame()
# for(i in seq(1,720,5)){
#   for(j in 1:360){
#     T.data =data.frame("Date" = Tmp.time1,
#                        "Year" = year(Tmp.time1),
#                        "month" = month(Tmp.time1),
#                        "Tem" = Tmp.data[i,j,])
#     T.data = T.data[which(T.data$Year>1959),]
#     if(length(which(complete.cases(T.data)))<12*50){
#       next
#     }
#     
#     ## Annual
#     A.T.data = aggregate(Tem~Year, data =T.data, mean)
#     names(A.T.data) = c("year","Annua.T")
#     T.data1 = T.data[which(T.data$month%in%c(6:8)),]
#     JJA.T.data = aggregate(Tem~Year, data =T.data1, mean)
#     names(JJA.T.data) = c("year","JJA.T")
#     T.data1 = T.data[which(T.data$month%in%c(11,12,1)),]
#     T.data1$Year[which(T.data1$month%in%c(11,12))] = 1 + T.data1$Year[which(T.data1$month%in%c(11,12))]
#     NDJ.T.data = aggregate(Tem~Year, data =T.data1, mean)
#     names(NDJ.T.data) = c("year","NDJ.T")
#     
#     ## Total
#     Total = merge(A.T.data,JJA.T.data)
#     Total = merge(Total,NDJ.T.data)
#     Total[,-1] = apply(Total[,-1], 2, pass.filter.NA.mean, W=10)
#     Total = merge(Total,CGR)
#     if(NROW(Total)<30){
#       next
#     }
#     
#     ## Correlation
#     Cor.data1 = data.frame()
#     Cor.data1[1,c("Lon","Lat")] = c(i.lon[i],i.lat[j])
#     temp1 = cor.test(Total$Annua.T, Total$Obs)
#     temp2 = cor.test(Total$JJA.T, Total$Obs)
#     temp3 = cor.test(Total$NDJ.T, Total$Obs)
#     Cor.data1[1,c("Cor.Annual","Cor.JJA","Cor.NDJ")] = c(temp1$estimate,temp2$estimate,temp3$estimate)
#     Cor.data1[1,c("Cor.p.Annual","Cor.p.JJA","Cor.p.NDJ")] = c(temp1$p.value,temp2$p.value,temp3$p.value)
#     Cor.data = rbind(Cor.data,Cor.data1)
#   }
#   print(i)
# }
# 
# ## Plot
# colors8=brewer.pal(n=8,"Spectral")
# world_map1 <-   ggplot() +
#   geom_polygon(data=con.boundary1,
#                color = "gray10", fill = "gray90",size=0.1,
#                aes(x = long, y = lat, group = group)) +
#   geom_point(
#     data = Cor.data[which(Cor.data$Cor.p.Annual<0.1),],
#     size = 2,
#     aes(x = Lon, y =Lat, color = Cor.Annual)
#   ) +
#   scale_color_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=9)+
#   scale_x_continuous(expand = c(0,0))+
#   coord_quickmap() + 
#   theme_bw()+
#   theme(
#     panel.grid = element_blank(),
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border=element_blank(),
#     legend.position =c("bottom"),
#     plot.margin =margin(4,4,0,4)  ## top, right, bottom, left
#   )
# 
# world_map2 <-   ggplot() +
#   geom_polygon(data=con.boundary1,
#                color = "gray10", fill = "gray90",size=0.1,
#                aes(x = long, y = lat, group = group)) +
#   geom_point(
#     data = Cor.data[which(Cor.data$Cor.p.JJA<0.1),],
#     size = 2,
#     aes(x = Lon, y =Lat, color = Cor.JJA)
#   ) +
#   scale_color_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=9)+
#   scale_x_continuous(expand = c(0,0))+
#   coord_quickmap() + 
#   theme_bw()+
#   theme(
#     panel.grid = element_blank(),
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border=element_blank(),
#     legend.position =c("bottom"),
#     plot.margin =margin(4,4,0,4)  ## top, right, bottom, left
#   )
# 
# world_map3 <-   ggplot() +
#   geom_polygon(data=con.boundary1,
#                color = "gray10", fill = "gray90",size=0.1,
#                aes(x = long, y = lat, group = group)) +
#   geom_point(
#     data = Cor.data[which(Cor.data$Cor.p.NDJ<0.1),],
#     size = 2,
#     aes(x = Lon, y =Lat, color = Cor.NDJ)
#   ) +
#   scale_color_stepsn(colors=colors8,limits=c(-0.62,0.62),n.breaks=9)+
#   scale_x_continuous(expand = c(0,0))+
#   coord_quickmap() + 
#   theme_bw()+
#   theme(
#     panel.grid = element_blank(),
#     panel.background = element_blank(),
#     axis.text = element_blank(),
#     axis.title = element_blank(),
#     axis.ticks = element_blank(),
#     panel.border=element_blank(),
#     legend.position =c("bottom"),
#     plot.margin =margin(4,4,0,4)  ## top, right, bottom, left
#   )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Annual Mean
# Annual.year =1850
# Climate.data.A= Climate.data[,,,which(month(Climate.time) == 1)]
# for(i in 2:12){
#   Climate.data.A = Climate.data.A+Climate.data[,,,which(month(Climate.time) == 1)]
# }
# Climate.data.A = Climate.data.A/12
# 
# ## 10-year high pass filter
# Climate.data.A.10 = apply(Climate.data.A,c(1,2,3),pass.filter.NA.mean, W=10)
# Climate.data.A.10 = aperm(Climate.data.A.10, c(2, 3, 4, 1))
# 
# ## Z-Score
# Climate.data.A.10.z = apply(Climate.data.A.10,c(1,2,3),scale)
# Climate.data.A.10.z = aperm(Climate.data.A.10.z, c(2, 3, 4, 1))



