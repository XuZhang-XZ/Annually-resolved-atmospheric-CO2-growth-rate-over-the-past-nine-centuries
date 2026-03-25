

# Read Climate Data --------------------------------------------------------------------

## Read Climate Data Pre~Tmp~TWS
Noah=nc_open("Z:/2022/Science9_SM/Output Data/Hydroclimate.nc")
Climate.data=ncvar_get(Noah,"TWS")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
nc_close(Noah)

# Read Land cover for IGBP ------------------------------------------------------

# ## Land covers based on IGBP
# ## https://doi.org/10.5067/MODIS/MCD12C1.061
# 1. Tropic Forests (1-5)
# 2. Tropic Shrublands  (6,7)
# 3. Tropic Savannas (8,9)
# 4. Tropic Grasslands  (10)
# 5. Extra-tropical Forests
# 6. Extra-tropical Shrublands
# 7. Extra-tropical Savannas
# 8. Extra-tropical Grasslands

## Read Land_Cover_Type_1
s <- sds("Input Data/Land Cover/MCD12C1/MCD12C1.A2001001.061.2022146170409.hdf")
tt = s[1]
Land.class = tt$Majority_Land_Cover_Type_1
temp1 = rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90,resolution = 0.5)
Land.class1 = terra::resample(x = Land.class,y=temp1,method = "mode")
LC.data <- expand.grid(lon = i.lon, lat = i.lat)
temp = terra::extract(x = Land.class1, y = as.matrix(LC.data[,1:2]), cells=FALSE)
LC.data$LAI.cover = temp$Majority_Land_Cover_Type_1

## Test
if(1 == 0) {
  p1 =
    ggplot() + geom_polygon(
      data = con.boundary1,
      color = "gray50",
      fill = NA,
      linewidth = 0.1,
      aes(x = long, y = lat, group = group)
    ) +
    geom_raster(
      data = LC.data %>% dplyr::filter(LAI.cover %in% c(12)),
      aes(
        x = lon,
        y = lat,
        fill = as.factor(LAI.cover)
      ),
      interpolate = FALSE
    )
  p1
}

## Group Land Cover
LC.data$New.LC = NA
LC.data$New.LC[which(LC.data$LAI.cover%in%c(1:5)&(LC.data$lat<(24)&LC.data$lat>(-24)))] = 1
LC.data$New.LC[which(LC.data$LAI.cover%in%c(6,7)&(LC.data$lat<(24)&LC.data$lat>(-24)))] = 2
LC.data$New.LC[which(LC.data$LAI.cover%in%c(8,9)&(LC.data$lat<(24)&LC.data$lat>(-24)))] = 3
LC.data$New.LC[which(LC.data$LAI.cover%in%c(10)&(LC.data$lat<(24)&LC.data$lat>(-24)))] = 4
LC.data$New.LC[which(LC.data$LAI.cover%in%c(1:5)&(LC.data$lat>=(24)|LC.data$lat<=(-24)))] = 5
LC.data$New.LC[which(LC.data$LAI.cover%in%c(6,7)&(LC.data$lat>=(24)|LC.data$lat<=(-24)))] = 6
LC.data$New.LC[which(LC.data$LAI.cover%in%c(8,9)&(LC.data$lat>=(24)|LC.data$lat<=(-24)))] = 7
LC.data$New.LC[which(LC.data$LAI.cover%in%c(10)&(LC.data$lat>=(24)|LC.data$lat<=(-24)))] = 8

## Names
New.LC.name = c(
  "Tropical forests",
  "Tropical shrublands",
  "Tropical savannas",
  "Tropical grasslands",
  "Extra-tropical forests",
  "Extra-tropical shrublands",
  "Extra-tropical savannas",
  "Extra-tropical grasslands"
)

## Colors
colors_NLC = c(
  rgb(50,205,49, maxColorValue = 255),
  rgb(194, 126, 127, maxColorValue = 255),
  rgb(223, 222, 0, maxColorValue = 255),
  rgb(224, 194, 123, maxColorValue = 255),
  rgb(157, 192, 68, maxColorValue = 255),
  rgb(189, 220, 189, maxColorValue = 255),
  rgb(228, 125, 124, maxColorValue = 255),
  rgb(225, 222, 131, maxColorValue = 255)
)

# Correlation for New land covers -----------------------------------------------------

## Data
Total.summary = read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")

## Locations
LC.data$ge.lon = sapply(LC.data$lon,function(x) {which(i.lon == x)})
LC.data$ge.lat = sapply(LC.data$lat,function(x) {which(i.lat == x)})

## PDSI Series
A.Weight = array(rep(cos(i.lat*pi/180),each = 720),c(720,360))
LC.ge =1

## PDSI Series
for(LC.ge in 1:length(New.LC.name)){
  
  ## Land Cover
  LC.data1 = LC.data[which(LC.data$New.LC==LC.ge),]
  Climate.data_Sel = array(NA,dim(Climate.data))
  
  ## Read
  for(j in 1:NROW(LC.data1)){
    Climate.data_Sel[,LC.data1$ge.lon[j],LC.data1$ge.lat[j],] = Climate.data[,LC.data1$ge.lon[j],LC.data1$ge.lat[j],]
  }
  
  ## Time Series
  PDSI.data = data.frame("year" = i.time,
                         "Pre" = areal.mean(z = Climate.data_Sel[1,,,],A.Weight = A.Weight),
                         "Tmp" = areal.mean(z = Climate.data_Sel[2,,,],A.Weight = A.Weight),
                         "PDSI" = areal.mean(z = Climate.data_Sel[3,,,],A.Weight = A.Weight))
  write.csv(PDSI.data,paste0("Output Data/Analysis/PDSI.Series/New.LC.", LC.ge , ".csv"))
}

## Correlation
PDSI.cor = data.frame()
LC.ge = 1

## Calculate Correlation
for(LC.ge in 1:length(New.LC.name)){
  
  ## Data
  PDSI.data = read.csv(paste0("Output Data/Analysis/PDSI.Series/New.LC.", LC.ge , ".csv"),row.names = 1)
  Total = merge(Total.summary,PDSI.data,by = "year")
  
  ## Reconstructed CGR (1901-2006)
  Total1 = Total[Total$year%in%c(1901:2006),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor1 = data.frame("Type" = c("Reconstructed CGR (1901-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Reconstructed CGR (1901-1958)
  Total1 = Total[Total$year%in%c(1901:1958),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor2 = data.frame("Type" = c("Reconstructed CGR (1901-1958)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Reconstructed CGR (1959-2006)
  Total1 = Total[Total$year%in%c(1959:2006),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor3 = data.frame("Type" = c("Reconstructed CGR (1959-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Observed CGR (1959-2006)
  Total1 = Total[Total$year%in%c(1959:2006),]
  temp1 = cor.test(Total1$Total.Obs,Total1$Pre)
  temp2 = cor.test(Total1$Total.Obs,Total1$Tmp)
  temp3 = cor.test(Total1$Total.Obs,Total1$PDSI)
  PDSI.cor4 = data.frame("Type" = c("Observed CGR (1959-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Combine
  PDSI.cor_T = rbind(PDSI.cor1,PDSI.cor2,PDSI.cor3,PDSI.cor4)
  PDSI.cor_T$LC.cover = LC.ge
  PDSI.cor_T$LC.cover.name = New.LC.name[LC.ge]
  
  ## Combine
  PDSI.cor = rbind(PDSI.cor,PDSI.cor_T)
  
}

## Write
write.csv(PDSI.cor,"Output Data/Figures/Cor.PDSI/PDSI.cor.csv")

## Plot
p_Land = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Pre"),
           aes(x = Cor,y = LC.cover.name,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single"))
p_Land = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Tmp"),
           aes(x = Cor,y = LC.cover.name,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single"))
p_Land = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "PDSI"),
           aes(x = Cor,y = LC.cover.name,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single"))


# Plot Correlations between PDSI and Pre and Tmp -------------------------------------------------------

## Data
PDSI.cor = read.csv("Output Data/Figures/Cor.PDSI/PDSI.cor.csv",row.names = 1)

## Significance
PDSI.cor$Cor1 = PDSI.cor$Cor
PDSI.cor$Cor1[PDSI.cor$Cor>0] = PDSI.cor$Cor[PDSI.cor$Cor>0]+0.02
PDSI.cor$Cor1[PDSI.cor$Cor<=0] = PDSI.cor$Cor[PDSI.cor$Cor<=0]-0.02
PDSI.cor$Cor1[PDSI.cor$p.Cor>=0.05] = 10

## Plot
Typess = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")
PDSI.cor = PDSI.cor[which(PDSI.cor$Type%in%Typess),]
PDSI.cor$Type = factor(PDSI.cor$Type, levels = Typess)
colors_P21=brewer.pal(n=8,"Set2")[c(1:4)]
labels_Xu = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")

## Plot
p_Land_1 = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Pre"),
           aes(x = LC.cover.name,y = Cor,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single")) +
  geom_point(
    data = PDSI.cor %>% dplyr::filter(Variable == "Pre"),
    aes(x = LC.cover.name,y = Cor1, color = Type),
    shape = 8,
    show.legend = F,
    size = 1,
    position = position_dodge(width = 0.47, preserve = "total")
  ) +
  geom_hline(yintercept = 0, size = unit(0.3, "pt"))+
  scale_y_continuous(
    limits = c(-1, 1),
    expand = c(0, 0),
    breaks = seq(-1, 1, 0.25),
    name = expression("Correlation with precipitation")
  ) +
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 5, 5, 4, "pt"),
        legend.position = "none",
        legend.justification.inside = c(1,0.8),
        axis.text.x.bottom = element_text(angle = 30, hjust = 1,vjust = 1),
        axis.title.x = element_blank())
p_Land_2 = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Tmp"),
           aes(x = LC.cover.name,y = Cor,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single")) +
  geom_point(
    data = PDSI.cor %>% dplyr::filter(Variable == "Tmp"),
    aes(x = LC.cover.name,y = Cor1, color = Type),
    shape = 8,
    show.legend = F,
    size = 1,
    position = position_dodge(width = 0.47, preserve = "total")
  ) +
  geom_hline(yintercept = 0, size = unit(0.3, "pt"))+
  scale_y_continuous(
    limits = c(-1, 1),
    expand = c(0, 0),
    breaks = seq(-1, 1, 0.25),
    name = expression("Correlation with temperature")
  ) +
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 5, 5, 4, "pt"),
        legend.position = "inside",
        legend.justification.inside = c(0.9,0.1),
        axis.text.x.bottom = element_text(angle = 30, hjust = 1,vjust = 1),
        axis.title.x = element_blank())

## Saving
p.total = plot_grid(p_Land_1,p_Land_2,ncol = 1, labels = "auto")

## Saving
ggsave("Figures/Analysis/Correlation_Land covers.tiff",
       p.total,
       width=7,
       height=7)
ggsave("Figures/Analysis/Correlation_Land covers.pdf",
       p.total,
       device = cairo_pdf,
       width=7,
       height=9)

# Plot Land Covers ------------------------------------------------

## Plot Six land covers
LC.data1 = LC.data[which(!is.na(LC.data$New.LC)),]
p31 = ggplot() + 
  geom_polygon(data=con.boundary1,
               color = "gray50", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_raster(data=LC.data1, aes(x=lon, y=lat, fill=as.factor(New.LC)),interpolate= FALSE)  + 
  scale_x_continuous(breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"))+
  scale_y_continuous(breaks = c(-60,0,60),labels = c("60°S","0","60°N"))+
  scale_fill_manual(values = colors_NLC,breaks = c(1:length(New.LC.name)), label = New.LC.name, guide = guide_legend(byrow= T))+
  coord_quickmap(xlim = c(-180,180), ylim = c(-60,85), expand = F) + 
  P_Xu_F1+
  theme(plot.margin = margin(5, 18, 5, 13, "pt"),
        legend.position = "bottom",
        axis.title = element_blank())

## Plot evergreen broad leaf forests
p32 = ggplot() + 
  geom_polygon(data=con.boundary1,
               color = "gray50", fill = NA,size=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_raster(data=LC.data[which(LC.data$LAI.cover == 2),], aes(x=lon, y=lat), fill = "green",interpolate= FALSE)  + 
  scale_x_continuous(limits = c(-180,180),expand = c(0,0),
                     breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"))+
  scale_y_continuous(limits = c(-60,85),expand = c(0,0),
                     breaks = c(-60,0,60),labels = c("60°S","0","60°N"))+
  coord_quickmap() + 
  P_Xu_F1+
  theme(plot.margin = margin(5, 18, 5, 13, "pt"),
        legend.position = "bottom",
        axis.title = element_blank())


## Saving
p.total = plot_grid(p31,p32,ncol = 1, labels = "auto",rel_heights = c(1.2,1))
ggsave("Figures/Analysis/evergreen broadleaf forests.pdf",
       p.total,
       device = cairo_pdf,
       dpi=450,
       width=9.30,
       height=8.52)

# Land cover for previous one -------------------------------------

## Read CGR Reconstructions
Total.summary = read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")

## Read Climate Data Pre~Tmp~TWS
Noah=nc_open("Z:/2022/Science9_SM/Output Data/Hydroclimate.nc")
Climate.data=ncvar_get(Noah,"TWS")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
i.time=ncvar_get(Noah, "time")
nc_close(Noah)

# ## Six class for LAI classification
# 1. Tropic Forests (5-8)
# 2. Extra-tropical Forests (5-8)
# 3. Semi-arid (2,4)
# 4. Tundra and arctic shrub land (2,4)
# 5. grasslands and Croplands (1, 3)
# 6. Sparsely-vegetated (9-10)
New.LC.name = c(
  "Tropical forests",
  "Extra-tropical forests",
  "Semi-arid",
  "Tundra and arctic shrublands",
  "Grasslands and croplands",
  "Sparsely-vegetated"
)
colors_NLC = c(
  rgb(164,200,244, maxColorValue = 255),
  rgb(244, 191, 129, maxColorValue = 255),
  rgb(223, 222, 0, maxColorValue = 255),
  rgb(30, 131, 61, maxColorValue = 255),
  rgb(158, 131, 61, maxColorValue = 255), ## 4. Extra-tropical Forest (5-8)
  rgb(30, 131, 61, maxColorValue = 255)
)

## Read Majority_Land_Cover_Type_3
s <- sds("Input Data/Land Cover/MCD12C1/MCD12C1.A2001001.061.2022146170409.hdf")
tt = s[7]
Land.class = tt$Majority_Land_Cover_Type_3
temp1 = rast(nrows=360, ncols=720, xmin=-180, xmax=180, ymin=-90, ymax=90,resolution = 0.5)
Land.class1 = terra::resample(x = Land.class,y=temp1,method = "mode")

## Land Covers
LC.data <- expand.grid(lon = i.lon, lat = i.lat)
temp = terra::extract(x = Land.class1, y = as.matrix(LC.data[,1:2]), cells=FALSE)
LC.data$LAI.cover = temp$Majority_Land_Cover_Type_3
LC.data$New.LC = NA
LC.data$New.LC[which(LC.data$LAI.cover%in%c(5:8)&(LC.data$lat<(24)&LC.data$lat>(-24)))] = 1
LC.data$New.LC[which(LC.data$LAI.cover%in%c(5:8)&(LC.data$lat>=(24)|LC.data$lat<=(-24)))] = 2
LC.data$New.LC[which(LC.data$LAI.cover%in%c(2,4)&(LC.data$lat<(45)))] = 3
LC.data$New.LC[which(LC.data$LAI.cover%in%c(2,4)&(LC.data$lat>=(45)))] = 4
LC.data$New.LC[which(LC.data$LAI.cover%in%c(1,3))] = 5
LC.data$New.LC[which(LC.data$LAI.cover%in%c(9:10))] = 6

## Locations
LC.data$ge.lon = sapply(LC.data$lon,function(x) {which(i.lon == x)})
LC.data$ge.lat = sapply(LC.data$lat,function(x) {which(i.lat == x)})

## Plot Six land covers
LC.data1 = LC.data[which(!is.na(LC.data$New.LC)),]
p31 = ggplot() + 
  geom_polygon(data=con.boundary1,
               color = "gray50", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_raster(data=LC.data1, aes(x=lon, y=lat, fill=as.factor(New.LC)),interpolate= FALSE)  + 
  scale_x_continuous(limits = c(-180,180),expand = c(0,0),
                     breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"))+
  scale_y_continuous(limits = c(-60,85),expand = c(0,0),
                     breaks = c(-60,0,60),labels = c("60°S","0","60°N"))+
  scale_fill_manual(values = colors_NLC,breaks = c(1:length(New.LC.name)), label = New.LC.name, guide = guide_legend(byrow= T))+
  coord_quickmap() + 
  P_Xu_F1+
  theme(plot.margin = margin(5, 18, 5, 13, "pt"),
        legend.position = "bottom",
        axis.title = element_blank())

## PDSI Series
A.Weight = array(rep(cos(i.lat*pi/180),each = 720),c(720,360))
LC.ge =1

## PDSI Series
for(LC.ge in 1:length(New.LC.name)){
  
  ## Land Cover
  LC.data1 = LC.data[which(LC.data$New.LC==LC.ge),]
  Climate.data_Sel = array(NA,dim(Climate.data))
  
  ## Read
  for(j in 1:NROW(LC.data1)){
    Climate.data_Sel[,LC.data1$ge.lon[j],LC.data1$ge.lat[j],] = Climate.data[,LC.data1$ge.lon[j],LC.data1$ge.lat[j],]
  }
  
  ## Time Series
  PDSI.data = data.frame("year" = i.time,
                         "Pre" = areal.mean(z = Climate.data_Sel[1,,,],A.Weight = A.Weight),
                         "Tmp" = areal.mean(z = Climate.data_Sel[2,,,],A.Weight = A.Weight),
                         "PDSI" = areal.mean(z = Climate.data_Sel[3,,,],A.Weight = A.Weight))
  write.csv(PDSI.data,paste0("Output Data/Analysis/PDSI.Series/Previous.LC.", LC.ge , ".csv"))
}

## Correlation
PDSI.cor = data.frame()
LC.ge = 1

## Calculate Correlation
for(LC.ge in 1:length(New.LC.name)){
  
  ## Data
  PDSI.data = read.csv(paste0("Output Data/Analysis/PDSI.Series/Previous.LC.", LC.ge , ".csv"),row.names = 1)
  Total = merge(Total.summary,PDSI.data,by = "year")
  
  ## Reconstructed CGR (1901-2006)
  Total1 = Total[Total$year%in%c(1901:2006),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor1 = data.frame("Type" = c("Reconstructed CGR (1901-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Reconstructed CGR (1901-1958)
  Total1 = Total[Total$year%in%c(1901:1958),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor2 = data.frame("Type" = c("Reconstructed CGR (1901-1958)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Reconstructed CGR (1959-2006)
  Total1 = Total[Total$year%in%c(1959:2006),]
  temp1 = cor.test(Total1$Pre.median,Total1$Pre)
  temp2 = cor.test(Total1$Pre.median,Total1$Tmp)
  temp3 = cor.test(Total1$Pre.median,Total1$PDSI)
  PDSI.cor3 = data.frame("Type" = c("Reconstructed CGR (1959-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Observed CGR (1959-2006)
  Total1 = Total[Total$year%in%c(1959:2006),]
  temp1 = cor.test(Total1$Total.Obs,Total1$Pre)
  temp2 = cor.test(Total1$Total.Obs,Total1$Tmp)
  temp3 = cor.test(Total1$Total.Obs,Total1$PDSI)
  PDSI.cor4 = data.frame("Type" = c("Observed CGR (1959-2006)"),
                         "Variable" = c("Pre","Tmp","PDSI"),
                         "Cor" = c(temp1$estimate,temp2$estimate,temp3$estimate),
                         "p.Cor" = c(temp1$p.value,temp2$p.value,temp3$p.value))
  
  ## Combine
  PDSI.cor_T = rbind(PDSI.cor1,PDSI.cor2,PDSI.cor3,PDSI.cor4)
  PDSI.cor_T$LC.cover = LC.ge
  PDSI.cor_T$LC.cover.name = New.LC.name[LC.ge]
  
  ## Combine
  PDSI.cor = rbind(PDSI.cor,PDSI.cor_T)
  
}

## Write
write.csv(PDSI.cor,"Output Data/Figures/Cor.PDSI/Previous_PDSI.cor.csv")

## Significance
PDSI.cor$Cor1 = PDSI.cor$Cor
PDSI.cor$Cor1[PDSI.cor$Cor>0] = PDSI.cor$Cor[PDSI.cor$Cor>0]+0.02
PDSI.cor$Cor1[PDSI.cor$Cor<=0] = PDSI.cor$Cor[PDSI.cor$Cor<=0]-0.02
PDSI.cor$Cor1[PDSI.cor$p.Cor>=0.05] = 10

## Plot Data
Typess = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")
PDSI.cor = PDSI.cor[which(PDSI.cor$Type%in%Typess),]
PDSI.cor$Type = factor(PDSI.cor$Type, levels = Typess)
colors_P21=brewer.pal(n=8,"Set2")[c(1:4)]
labels_Xu = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")

## Plot Correlations
p_Land_1 = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Pre"),
           aes(x = LC.cover.name,y = Cor,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single")) +
  geom_point(
    data = PDSI.cor %>% dplyr::filter(Variable == "Pre"),
    aes(x = LC.cover.name,y = Cor1, color = Type),
    shape = 8,
    show.legend = F,
    size = 1,
    position = position_dodge(width = 0.47, preserve = "total")
  ) +
  geom_hline(yintercept = 0, size = unit(0.3, "pt"))+
  scale_y_continuous(limits = c(-0.8,0.8),expand = c(0,0),breaks = seq(-1,1,0.25),
                     name = expression("Correlation with precipitation"))+
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 5, 5, 4, "pt"),
        legend.position = "inside",
        legend.justification.inside = c(0.9,0.9),
        axis.text.x.bottom = element_text(angle = 30, hjust = 1,vjust = 1),
        axis.title.x = element_blank())
p_Land_2 = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "Tmp"),
           aes(x = LC.cover.name,y = Cor,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single")) +
  geom_point(
    data = PDSI.cor %>% dplyr::filter(Variable == "Tmp"),
    aes(x = LC.cover.name,y = Cor1, color = Type),
    shape = 8,
    show.legend = F,
    size = 1,
    position = position_dodge(width = 0.47, preserve = "total")
  ) +
  geom_hline(yintercept = 0, size = unit(0.3, "pt"))+
  scale_y_continuous(limits = c(-0.8,0.8),expand = c(0,0),breaks = seq(-1,1,0.25),
                     name = expression("Correlation with temperature"))+
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 5, 5, 4, "pt"),
        legend.position = "inside",
        legend.justification.inside = c(0.9,0.9),
        axis.text.x.bottom = element_text(angle = 30, hjust = 1,vjust = 1),
        axis.title.x = element_blank())
p_Land_3 = ggplot()+
  geom_col(data = PDSI.cor %>% dplyr::filter(Variable == "PDSI"),
           aes(x = LC.cover.name,y = Cor,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single")) +
  geom_point(
    data = PDSI.cor %>% dplyr::filter(Variable == "PDSI"),
    aes(x = LC.cover.name,y = Cor1, color = Type),
    shape = 8,
    show.legend = F,
    size = 1,
    position = position_dodge(width = 0.47, preserve = "total")
  ) +
  geom_hline(yintercept = 0, size = unit(0.3, "pt"))+
  scale_y_continuous(limits = c(-0.8,0.8),expand = c(0,0),breaks = seq(-1,1,0.25),
                     name = expression("Correlation with scPDSI"))+
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 5, 5, 4, "pt"),
        legend.position = "inside",
        legend.justification.inside = c(0.9,0.9),
        axis.text.x.bottom = element_text(angle = 30, hjust = 1,vjust = 1),
        axis.title.x = element_blank())

## Saving
ggsave("Figures/Analysis/Cor with semi-arid.tiff",
       p_Land_3,
       width=7,
       height=5)

# End -------------------------------------------------------------
