

# Read correlations with temperature observations -----------------------------------------

## Data
Cor.HadCRUT_Ocean = read.csv("Output Data/CGR/Cor with HadCRUT.csv")
Cor.HadCRUT_Land = read.csv("Output Data/CGR/Cor with CRU_Tmp.csv")
Cor_Obs_T = rbind(Cor.HadCRUT_Ocean, Cor.HadCRUT_Land)
Cor_Obs_T = Cor_Obs_T[complete.cases(Cor_Obs_T), ]

## PAGES 2k predictors
properties1 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/Cor.properties.select.csv")
ge=1

## Find max Correlation with distance < 1000 km
for(ge in 1:NROW(properties1)){
  
  ## Regional Points
  Dis = 111*cos(properties1$Lat[ge]*pi/180)
  
  ## Period
  Cor_Obs_T1 = Cor_Obs_T %>%
    dplyr::filter(
        Lon > (properties1$Lon[ge] - 10) &
        Lon < (properties1$Lon[ge] + 10) &
        Lat > (properties1$Lat[ge] - 1400 / Dis) &
        Lat < (properties1$Lat[ge] + 1400 / Dis)
    )
  
  ## Locations
  for (k in 1:NROW(Cor_Obs_T1)) {
    Cor_Obs_T1[k, "distance"] = distanceme(
      lon1 = properties1$Lon[ge],
      lat1 = properties1$Lat[ge],
      lon2 = Cor_Obs_T1$Lon[k],
      lat2 = Cor_Obs_T1$Lat[k]
    )
  }
  Cor_Obs_T_nearst = Cor_Obs_T1[which(Cor_Obs_T1$distance < 1000), ]
  
  ## Check
   if(NROW(Cor_Obs_T_nearst) <=2) {
     next
   }
  
  ## Max Correlation in Current Year
  Ge_Sel = which.max(Cor_Obs_T_nearst$A.0)
  properties1[ge,"P.Obs.Cor.0"] = Cor_Obs_T_nearst$A.0[Ge_Sel]
  properties1[ge,"P.Obs.Cor.0.p"] = Cor_Obs_T_nearst$A.p.0[Ge_Sel]
  Ge_Sel = which.min(Cor_Obs_T_nearst$A.0)
  properties1[ge,"N.Obs.Cor.0"] = Cor_Obs_T_nearst$A.0[Ge_Sel]
  properties1[ge,"N.Obs.Cor.0.p"] = Cor_Obs_T_nearst$A.p.0[Ge_Sel]
  
  ## Max Correlation in Preceding Year
  Ge_Sel = which.max(Cor_Obs_T_nearst$A.1)
  properties1[ge,"P.Obs.Cor.1"] = Cor_Obs_T_nearst$A.1[Ge_Sel]
  properties1[ge,"P.Obs.Cor.1.p"] = Cor_Obs_T_nearst$A.p.1[Ge_Sel]
  Ge_Sel = which.min(Cor_Obs_T_nearst$A.1)
  properties1[ge,"N.Obs.Cor.1"] = Cor_Obs_T_nearst$A.1[Ge_Sel]
  properties1[ge,"N.Obs.Cor.1.p"] = Cor_Obs_T_nearst$A.p.1[Ge_Sel]
  
  ## Print
  print(ge)
}

# Selection based on correlations with regional temperature -------------------------------------------------------

## Data
ge = 1

## Compare
for(ge in 1:NROW(properties1)){
  
  ## Data
  properties1[ge, paste0("Select.", c(0,1))] = FALSE
  
  ## Current Year
  if(properties1$A.Cor.0[ge] > 0) {
    properties1[ge,"Obs.Cor.0"] = properties1[ge,"P.Obs.Cor.0"]
    properties1[ge,"Obs.Cor.0.p"] = properties1[ge,"P.Obs.Cor.0.p"]
  }
  if(properties1$A.Cor.0[ge] <= 0) {
    properties1[ge,"Obs.Cor.0"] = properties1[ge,"N.Obs.Cor.0"]
    properties1[ge,"Obs.Cor.0.p"] = properties1[ge,"N.Obs.Cor.0.p"]
  }
  if (is.na(properties1$Obs.Cor.0[ge]) & properties1[ge, "A.Cor.p.0"] < 0.05) {
    properties1[ge, "Select.0"] = TRUE
  } else {
    if (sign(properties1$A.Cor.0[ge]) * sign(properties1$Obs.Cor.0[ge]) == 1 &
        properties1[ge, "A.Cor.p.0"] < 0.05 &
        properties1[ge, "Obs.Cor.0.p"] < 0.05) {
      properties1[ge, "Select.0"] = TRUE
    }
  }
  
  ## Preceding Year
  if(properties1$A.Cor.1[ge] > 0) {
    properties1[ge,"Obs.Cor.1"] = properties1[ge,"P.Obs.Cor.1"]
    properties1[ge,"Obs.Cor.1.p"] = properties1[ge,"P.Obs.Cor.1.p"]
  }
  if(properties1$A.Cor.1[ge] <= 0) {
    properties1[ge,"Obs.Cor.1"] = properties1[ge,"N.Obs.Cor.1"]
    properties1[ge,"Obs.Cor.1.p"] = properties1[ge,"N.Obs.Cor.1.p"]
  }
  if (is.na(properties1$Obs.Cor.1[ge]) & properties1[ge, "A.Cor.p.1"] < 0.05) {
    properties1[ge, "Select.1"] = TRUE
  } else {
    if (sign(properties1$A.Cor.1[ge]) * sign(properties1$Obs.Cor.1[ge]) == 1 &
        properties1[ge, "A.Cor.p.1"] < 0.05 &
        properties1[ge, "Obs.Cor.1.p"] < 0.05) {
      properties1[ge, "Select.1"] = TRUE
    }
  }
  
  ## Select current or previous year
  Sel.ge = NA
  if (properties1[ge, "Select.0"] == TRUE & properties1[ge, "Select.1"] == FALSE) {
    Sel.ge = 0
  }
  if (properties1[ge, "Select.0"] == FALSE & properties1[ge, "Select.1"] == TRUE) {
    Sel.ge = 1
  }
  if (properties1[ge, "Select.0"] == TRUE & properties1[ge, "Select.1"] == TRUE) {
    Sel.ge = which.max(abs(properties1[ge, c("A.Cor.0", "A.Cor.1")])) - 1
  }
  
  ## Output
  if(is.na(Sel.ge)) {
    next
  }
  if(Sel.ge == 0) {
    properties1[ge, "Select.ge"] = 0
    properties1[ge, "Select.cor"] = properties1$A.Cor.0[ge]
    properties1[ge, "Select.cor.p"] = properties1$A.Cor.p.0[ge]
    properties1[ge, "Select.Obs.cor"] = properties1$Obs.Cor.0[ge]
    properties1[ge, "Select.Obs.cor.p"] = properties1$Obs.Cor.0.p[ge]
  }
  if(Sel.ge == 1) {
    properties1[ge, "Select.ge"] = 1
    properties1[ge, "Select.cor"] = properties1$A.Cor.1[ge]
    properties1[ge, "Select.cor.p"] = properties1$A.Cor.p.1[ge]
    properties1[ge, "Select.Obs.cor"] = properties1$Obs.Cor.1[ge]
    properties1[ge, "Select.Obs.cor.p"] = properties1$Obs.Cor.1.p[ge]
  }
}

## Selection
properties2 = properties1 %>% dplyr::filter(!is.na(Select.ge))

## Write
write.csv(properties2,"Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv",row.names = FALSE)

# Output Predictors -------------------------------------------------------

## Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")

## Revise Column
properties3 = properties2 %>%
  dplyr::arrange(pages2kID) %>%
  dplyr::select(pages2kID,Lon,Lat,archive,Select.ge,Select.cor,Select.Obs.cor) %>%
  dplyr::mutate(Lon = sprintf("%.1f",Lon),
                Lat = sprintf("%.1f",Lat),
                Select.cor = sprintf("%.2f",Select.cor),
                Select.Obs.cor = sprintf("%.2f",Select.Obs.cor))

## Change preceding year
properties3$Select.ge[which(properties3$Select.ge == 1)] = -1

## Write
write_xlsx(properties3,path = "Output Data/Predictors/Table_S2_temp.xlsx")

# Read Predictors ------------------------------------------------------

## Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
predictors=data.frame("year"= c(1:2030))
ge = 1

## Read Series
for(ge in 1:NROW(properties2)){
  
  ## read chronology
  raw.chro = read.csv(
    file = paste0(
      "Output Data/PAGES 2k v.2.0.0/High_pass_PAGES/",
      properties2$pages2kID[ge],
      ".csv"
    ),
    row.names = 1
  )
  names(raw.chro) = c("year",properties2$pages2kID[ge])
  
  ## Change Years
  raw.chro$year = raw.chro$year + properties2[ge, "Select.ge"]
  
  ## Write
  write.csv(raw.chro,paste0("Output Data/PAGES 2k v.2.0.0/Final Predictors/",properties2$pages2kID[ge],".csv"),row.names = F)
  
  ## Combine
  predictors=merge(predictors,raw.chro,all.x = T)
}

# Calculate percentage of missing values
as=as.matrix(predictors[which(predictors$year%in%c(1959:2000)),-1])

## Write
write.csv(predictors,"Output Data/Predictors/PAGS2k.csv",row.names = FALSE)

# Plot Locations of Proxies ----------------------------------------------------

## Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
colors8=brewer.pal(n=11,"Spectral")[11:1]

## Plot
p1 <-   ggplot() +
  geom_point(
    data = properties2 %>% dplyr::filter(Select.ge == 0),
    aes(x = Lon, y =Lat, color = Select.cor)
  ) +
  geom_polygon(data=con.boundary1,
               color = "gray10", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_hline(yintercept = c(-30,30), col = "blue", linetype = "dashed")+
  scale_color_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
  coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0))
p1 <-   ggplot() +
  geom_point(
    data = properties2 %>% dplyr::filter(Select.ge == 1),
    aes(x = Lon, y =Lat, color = Select.cor)
  ) +
  geom_polygon(data=con.boundary1,
               color = "gray10", fill = NA,linewidth=0.1,
               aes(x = long, y = lat, group = group)) +
  geom_hline(yintercept = c(-30,30), col = "blue", linetype = "dashed")+
  scale_color_stepsn(colors=colors8,limits=c(-0.62,0.62),breaks = round(seq(-0.6,0.6,0.2),1))+
  coord_quickmap(xlim = c(-180,180),ylim = c(-85,85),expand = c(0.0))


# Plot Series of Proxies ----------------------------------------------------

## Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
ge = 1

## Plot
for(ge in 1:NROW(properties2)) {
  
  ## read chronology
  raw.chro = read.csv(
    file = paste0(
      "Output Data/PAGES 2k v.2.0.0/High_pass_PAGES/",
      properties2$pages2kID[ge],
      ".csv"
    ),
    row.names = 1
  )
  
  ## Plot
  jpeg(filename = paste0("Figures/Predictors/Series/",properties2$pages2kID[ge],".jpeg"),width = 880, height = 1080)
  plot(raw.chro$year,raw.chro$value,type = "l", xlim = c(1000,2000))
  dev.off()
  
}

# Plot Number over time ------------------------------------------------

## Number of Chronologies
predictors=read.csv("Output Data/Predictors/PAGS2k.csv")
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")

## Archive
Name.Archive=unique(properties2$archive)
Number=data.frame()
for(i in 1:length(Name.Archive)){
  predictors1=predictors[,c(1,which(properties2$archive==Name.Archive[i])+1)]
  Number1=data.frame("year"=predictors$year,
                     "Num"=apply(predictors1,1,function(a) {length(which(!is.na(a)))})-1,
                     "Archive"=Name.Archive[i])
  Number=rbind(Number,Number1)
}

p1=ggplot()+
  geom_area(data=Number,
            aes(x=year,y=Num,fill=Archive))+
  scale_x_continuous(expand = c(0.02,0.02))+
  coord_cartesian(xlim = c(0,2020))+
  scale_y_continuous(limits = c(0,65),expand = c(0.01,0.01))+
  ylab("Number")+
  scale_fill_discrete()+
  theme_bw()+
  theme(legend.position = c(0.3,0.65),
        legend.background = element_blank(),
        legend.title = element_blank())

p2=ggplot()+
  geom_area(data=Number,
            aes(x=year,y=Num,fill=Archive))+
  scale_x_continuous(expand = c(0.02,0.02),breaks = seq(1900,2000,10))+
  coord_cartesian(xlim = c(1900,2020))+
  scale_y_continuous(limits = c(0,65),expand = c(0.01,0.01))+
  ylab("Number")+
  scale_fill_discrete()+
  theme_bw()+
  theme(legend.position = "none",
        legend.background = element_blank(),
        legend.title = element_blank())

p.total=plot_grid(p1,p2,labels = "auto",
                  nrow=2)
ggsave("Figures/Predictors/Number.jpeg",
       p.total,
       dpi=450,
       width=8,
       height=7)


# End -------------------------------------------------------------

