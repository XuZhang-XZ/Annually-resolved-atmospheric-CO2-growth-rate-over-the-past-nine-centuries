

# Read Climate Data ------------------------------------------------------------

## CRU P
Noah = nc_open("Z:/2023/Input data/CRU TS v4.07/cru_ts4.07.1901.2022.pre.dat.nc")
Pre.data = ncvar_get(Noah, "pre")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(19000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
nc_close(Noah)

## CRU T
Noah = nc_open("Z:/2023/Input data/CRU TS v4.07/cru_ts4.07.1901.2022.tmp.dat.nc")
Tmp.data = ncvar_get(Noah, "tmp")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(19000101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
nc_close(Noah)

## Global PDSI
Noah = nc_open("Z:/2023/Input data/CRU_PDSI_4.07/scPDSI.cru_ts4.07early1.1901.2022.cal_1901_22.bams.2023.GLOBAL.IGBP.WHC.1901.2022.nc")
TWS.data=ncvar_get(Noah,"scpdsi")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
TWS.time=ncvar_get(Noah, "time")
TWS.time1=TWS.time+ymd(19000101)
TWS.time1 = ymd(year(TWS.time1)*10000+month(TWS.time1)*100+1)
nc_close(Noah)

## Change Coordinates
i.lat=i.lat[360:1]
TWS.data = TWS.data[,c(360:1),]

## Total for Pre~Tmp~TWS
i.year = c(1901:2006)
Total.data = array(NA,c(3,length(i.lon),length(i.lat),length(i.year)))
Total.data[1,,,] = Pre.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == 1)]
Total.data[2,,,] = Tmp.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == 1)]
Total.data[3,,,] = TWS.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == 1)]
for(ge.mon in 2:12){
  Total.data[1,,,] = Total.data[1,,,] + Pre.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == ge.mon)]
  Total.data[2,,,] = Total.data[2,,,] + Tmp.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == ge.mon)]
  Total.data[3,,,] = Total.data[3,,,] + TWS.data[,,which(year(Tmp.time1)%in%i.year&month(Tmp.time1) == ge.mon)]
}
Total.data[1,,,] = Total.data[1,,,]
Total.data[2,,,] = Total.data[2,,,]/12
Total.data[3,,,] = Total.data[3,,,]/12

## Testing
if(1 == 0) {
  image.plot(Total.data[1,,,1])
  image.plot(Total.data[2,,,1])
  image.plot(Total.data[3,,,1])
}

## 10-year high pass filter
Total.data = apply(Total.data,c(1,2,3),pass.filter.NA.mean, W=10)
Total.data = aperm(Total.data, c(2, 3, 4, 1))

## Write Data set
ge <- ncdim_def(name = "ge", units = "Pre~Tmp~TWS", vals = 1:3)
x <- ncdim_def(name = "lon", units = "degrees_east", vals = i.lon)
y <- ncdim_def(name = "lat", units = "degrees_north", vals = i.lat)
t <- ncdim_def(name = "time", units = "years CE", vals = i.year, unlim = TRUE)
var1 <- ncvar_def("TWS", "mm", list(ge, x, y, t), missval=NA, prec = "float")
vars <- list(var1)
ncnew <- nc_create("Z:/2022/Science9_SM/Output Data/Hydroclimate.nc", vars)
ncvar_put(ncnew, var1, Total.data)
ncatt_put(ncnew, "lon", "standard_name", "longitude", prec = "text")
ncatt_put(ncnew, "lat", "standard_name", "latitude", prec = "text")
nc_close(ncnew)

# Gridded Correlations -----------------------------------------------------

## Global Climate Data
Noah = nc_open("Z:/2022/Science9_SM/Output Data/Hydroclimate.nc")
Total.data = ncvar_get(Noah, "TWS")
i.lon = ncvar_get(Noah, "lon")
i.lat = ncvar_get(Noah, "lat")
i.year = ncvar_get(Noah, "time")
nc_close(Noah)

## Reconstructed CGR
Total.summary = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary = Total.summary[Total.summary$year%in% c(1901:2020), ]
Total.summary$Pre.median[which(Total.summary$year%in%c(1991:1992))] = NA
Total.summary$Pre.median[which(Total.summary$year%in%c(2007:2020))] = NA
Total.summary$Total.Obs[which(Total.summary$year%in%c(1991:1992))] = NA

## Correlation for Pre~Tmp~TWS
Cor.CGR = array(NA,c(2,3,720,360))
Cor.TWS = array(NA,c(2,2,720,360))
lons = 300;lats = 300
Var = 1

## Correlation
for(lons in 1:720){
  for(lats in 1:360){
    
    ## Cor with CGR
    for(ge in 1:3){
      temp1 = cor30(Total.summary$Pre.median[which(Total.summary$year%in%c(1901:2006))],
                    Total.data[ge,lons,lats,])
      Cor.CGR[,ge,lons,lats] = c(temp1$estimate,temp1$p.value)
    }
    
    ## Cor PDSI and Pre
    temp1 = cor30(Total.data[3,lons,lats,], Total.data[1,lons,lats,])
    Cor.TWS[,1,lons,lats] = c(temp1$estimate,temp1$p.value)
    
    ## Cor PDSI and Tmp
    temp1 = cor30(Total.data[3,lons,lats,], Total.data[2,lons,lats,])
    Cor.TWS[,2,lons,lats] = c(temp1$estimate,temp1$p.value)
    
  }
  print(lons)
}

## Output Cor with CGR
mesh.Cor<- expand.grid(lon = i.lon, lat = i.lat)
mesh.Cor$Cor.Pre <- c(Cor.CGR[1,1,,])
mesh.Cor$Cor.Tmp <- c(Cor.CGR[1,2,,])
mesh.Cor$Cor.TWS <- c(Cor.CGR[1,3,,])
mesh.Cor$Cor.Pre.p <- c(Cor.CGR[2,1,,])
mesh.Cor$Cor.Tmp.p <- c(Cor.CGR[2,2,,])
mesh.Cor$Cor.TWS.p <- c(Cor.CGR[2,3,,])

## Output Cor with PDSI
mesh.Cor$TWS.Pre <- c(Cor.TWS[1,1,,])
mesh.Cor$TWS.Pre.p <- c(Cor.TWS[2,1,,])
mesh.Cor$TWS.Tmp <- c(Cor.TWS[1,2,,])
mesh.Cor$TWS.Tmp.p <- c(Cor.TWS[2,2,,])

## Write
write.csv(mesh.Cor,"Output Data/Figures/Cor.PDSI/mesh.Cor_3.csv")

# Plot Test Results ---------------------------------------------------------------------

## P - value
i.lon1 = seq(-179.75,179.75,2.5)
i.lat1 = seq(-89.75,89.75,2.5)
mesh.Cor.p = mesh.Cor[mesh.Cor$lon%in%i.lon1&mesh.Cor$lat%in%i.lat1,]

## Theme
P_Xu_F3 = theme(
  plot.margin = margin(5, 10, 5, 10, "pt"),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_rect(size = unit(0.5, "pt"),fill = NA, color = "black"),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_text(color= "black"),
  axis.title = element_text(color= "black"),
  legend.title = element_blank(),
  legend.background = element_blank(),
  legend.key = element_blank()
)

## Plot Correlation
color.Cor<- brewer.pal(n=11,"RdYlBu")[11:1]
cols.plot = c("Cor.Pre","Cor.Tmp","Cor.TWS","TWS.Pre","TWS.Tmp")
cols.plot.p = c("Cor.Pre.p","Cor.Tmp.p","Cor.TWS.p","TWS.Pre.p","TWS.Tmp.p")
pplot.p = paste0("Cor_p_",c(1:5))
i = 1
for(i in 1:5){
  assign(pplot.p[i],
         p11 <- ggplot() + 
           geom_polygon(data=con.boundary1,
                        color = "gray50", fill = NA,linewidth=0.1,
                        aes(x = long, y = lat, group = group)) +
           geom_raster(data=mesh.Cor, aes( x=lon, y=lat, fill=.data[[cols.plot[i]]]),interpolate= FALSE)  + 
           geom_point( data=mesh.Cor.p[which(mesh.Cor.p[,cols.plot.p[i]]<0.05),], aes( x=lon, y=lat),shape = 16,size= 0.3,alpha = 1 , 
                       color = rgb(0,0,0, maxColorValue = 255))  +
           scale_fill_stepsn(colours = color.Cor,limits=c(-0.52,0.52),breaks=seq(-0.5,0.5,0.1),na.value =NA)+
           scale_x_continuous(limits = c(-180.3,180.3),expand = c(0,0),
                              breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"))+
           scale_y_continuous(limits = c(-61,85),expand = c(0,0),
                              breaks = c(-60,0,60),labels = c("60°S","0","60°N"))+
           xlab(" ")+ylab(" ")+
           coord_quickmap() + 
           P_Xu_F3+
           theme(legend.position = "none",
                 axis.text = element_blank(),
                 axis.title = element_blank())
         )
}

## Legend
legend1 <-   ggplot() + 
  geom_raster(data=mesh.Cor, aes( x=lon, y=lat, fill=.data[[cols.plot[i]]]),interpolate= FALSE)  + 
  scale_fill_stepsn(colours = color.Cor,limits=c(-0.42,0.42),breaks=seq(-0.4,0.4,0.1),na.value =NA)+
  theme(
    plot.margin = margin(2, 5, 2, 2, "pt"),
    legend.position =c(0.5,0.52),
    legend.key.width = unit(70,"pt"),
    legend.key.height = unit(5,"pt"),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )
legend.Cor <- plot_grid(get_legend(legend1))

## Saving
p.left = plot_grid(
  Cor_p_1,Cor_p_2,
  Cor_p_4,Cor_p_5,
  ncol = 2,
  labels = "auto",
  label_x = 0.05,label_y = 0.27
)
p.total = plot_grid(p.left,legend.Cor,nrow = 2,rel_heights = c(1,0.07))
ggsave(plot = p.total,
       filename = "Figures/Analysis/Correlation with Climate.pdf",
       device = cairo_pdf,
       width=9.3,
       height=4.5)

# End -------------------------------------------------------------

