

# CO2 Measurements ------------------------------------------------

## Jinho Measurements
Jinho.2012 = read.table("Input Data/Historical CO2 Concentration/Jinho Ahn.2012/wais2012co2-noaa.txt",sep = "\t",header = T,skip = 122)
Jinho.2012 = Jinho.2012[,c(2,3,4)]
names(Jinho.2012) = c("year","CO2","OneS")
Jinho.2012$Type = "WAIS"

## Rubino Measurements
Ice.CO2 = read.table("Input Data/Historical CO2 Concentration/Rubino.2018/CO2 measurement.txt",sep = "\t",header = T)
Ice.CO2 = Ice.CO2[,c(3,4,5)]
names(Ice.CO2) = c("year","CO2","OneS")
Ice.CO2$Type = "Law Dome"

## Total
CO2.ice.points = rbind(Jinho.2012,Ice.CO2)
write.csv(CO2.ice.points,"Output Data/Reconstructed CGR/SD/CO2.ice.points.csv",row.names = F)

# Tropical Temperature in HadCRUT -----------------------------------------

## Tropical Temperature
Noah=nc_open("Z:/2023/Input data/HadCRUT5/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc")
Tmp.data=ncvar_get(Noah,"tas_mean")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(18500101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
nc_close(Noah)

## Annual Temperature
Tmp.data[,which(i.lat>(23)|i.lat<(-23)),] = NA
A.Weight = array(rep(cos(i.lat*pi/180),each = 72),c(72,36))
Tropical.T.data = data.frame("year" = year(Tmp.time1),
                             "month" = month(Tmp.time1),
                             "Tropical.T" = areal.mean(z = Tmp.data,A.Weight = A.Weight))
Annual.Tropical.T=aggregate(Tropical.T~year,data=Tropical.T.data,mean,na.rm=TRUE)
Annual.Tropical.T$Tropical.T.10=pass.filter.NA(a=Annual.Tropical.T$Tropical.T,W=10,type="high",method = c("Butterworth"))
write.csv(Annual.Tropical.T,"Output Data/Reconstructed CGR/SD/Annual.Tropical.T.csv",row.names = F)

# Tropical proxies in PAGES 2k ------------------------------------

## PAGES 2k Series
PAGES.series_Raw = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/All.properties.csv")

## Filter
PAGES.series = PAGES.series_Raw %>%
  dplyr::filter(Resolution_median > 1) %>%
  dplyr::filter(Lat < 24 & Lat > (-24)) %>%
  dplyr::filter(Start.year <= 1100 & End.year >= 1800)

## Read Total Proxy
All.series = data.frame("year" = c(1100:2020))
i = 7

## Read
for(i in 1:NROW(PAGES.series)){
  
  ## Read
  read.name = paste0("Output Data/PAGES 2k v.2.0.0/Annual.Processded Data/",PAGES.series$pages2kID[i],".csv")
  Data.series = read.csv(read.name,row.names = 1)
  
  ## Filling missing values
  Data.series = filling.missing(Data.series)
  names(Data.series) = c("year",PAGES.series$pages2kID[i])
  
  ## Total
  All.series = merge(All.series,Data.series,all.x = T)
}

## Standardization
All.series1 = All.series
Common.rows = complete.cases(All.series1)
for(i in 2:NCOL(All.series1)) {
  row_mean = mean(All.series1[Common.rows,i])
  row_sd = sd(All.series1[Common.rows,i])
  All.series1[,i] = (All.series1[,i] - row_mean) / row_sd
}

## Average
All.series1$Average.T = rowMeans(All.series1[,-1], na.rm = T)
All.series2 = All.series1[which(All.series1$year%in%c(1100:1980)),]

## Read Observed Tropical Temperature
Annual.Tropical.T = read.csv("Output Data/Reconstructed CGR/SD/Annual.Tropical.T.csv")
Total.Tropical.T = data.frame("year" = c(1100:2020))
Total.Tropical.T = merge(Total.Tropical.T,All.series2[,c("year","Average.T")],all.x = T)
Total.Tropical.T = merge(Total.Tropical.T,Annual.Tropical.T[,c("year","Tropical.T")],all.x = T)

## Re-scale to tropical T
Total.Tropical.T_Sel = Total.Tropical.T[which(Total.Tropical.T$year%in%c(1850:1980)),]
a1.mean = mean(Total.Tropical.T_Sel$Average.T)
a1.sd = sd(Total.Tropical.T_Sel$Average.T)
a2.mean = mean(Total.Tropical.T_Sel$Tropical.T)
a2.sd = sd(Total.Tropical.T_Sel$Tropical.T)
Total.Tropical.T$Tropical.T_Rec = (Total.Tropical.T$Average.T - a1.mean)/a1.sd*a2.sd+a2.mean

## 11-year low-pass filter
Total.Tropical.T = Total.Tropical.T %>% 
  dplyr::mutate(across(!year, ~ pass.filter.NA(.x, W = 11, type = "low")))

## Uncertainty by SD
SD.tropic.T = sd(Total.Tropical.T$Tropical.T_Rec - Total.Tropical.T$Tropical.T,na.rm = TRUE)
Total.Tropical.T = Total.Tropical.T %>% 
  dplyr::mutate(Upper.T = Tropical.T_Rec + 2*SD.tropic.T,
                Lower.T = Tropical.T_Rec - 2*SD.tropic.T)

## Replace Obs to Rec
Sel_row = which(Total.Tropical.T$year %in% c(1981:2020))
Total.Tropical.T$Tropical.T_Rec[Sel_row] = Total.Tropical.T$Tropical.T[Sel_row]

## Write
write_xlsx(PAGES.series,"Output Data/Predictors/Tropical_PAGES.series.xlsx")
write.csv(Total.Tropical.T,"Output Data/Reconstructed CGR/SD/Total_Tropic.csv",row.names = F)

## Plot
plot(Total.Tropical.T$year,Total.Tropical.T$Tropical.T,type = "l")
lines(Total.Tropical.T$year,Total.Tropical.T$Tropical.T_Rec,col = "blue")
lines(Total.Tropical.T$year,Total.Tropical.T$Upper.T,col = "red")
lines(Total.Tropical.T$year,Total.Tropical.T$Lower.T,col = "red")

# Correlation between Tropical T and CGR SD ------------------------------

## Data
colors_temp2 = brewer.pal(n=9,"Set1")
Total_Tropic = read.csv("Output Data/Reconstructed CGR/SD/Total_Tropic.csv")
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total = merge(Total_Tropic,Total.summary.sd, by = "year",all.x = T)

## Average 31 years
Total$HR.SD.median[which(Total$year %in% c(1992:2005))] = Total$Obs.CGR.Var[which(Total$year %in% c(1992:2005))]
Total = Total[Total$year %in% c(1100:2005),]
Total$New_year = ceiling((Total$year) / 31) * 31
Total1 = aggregate(
  cbind(Tropical.T_Rec, HR.SD.median) ~ New_year,
  data = Total,
  mean,
  na.rm = T
)

## Plot
plot(Total1$New_year,scale(Total1$Tropical.T_Rec),type = "l")
lines(Total1$New_year,scale(Total1$HR.SD.median),type = "l",col = "red")
cor.test(Total1$Tropical.T_Rec,Total1$HR.SD.median)

## Data
Total1$Tropical.T_Rec = as.numeric(scale(Total1$Tropical.T_Rec))
Total1$HR.SD.median = as.numeric(scale(Total1$HR.SD.median))
plot.data = pivot_longer(data = Total1, cols = c("Tropical.T_Rec","HR.SD.median"))

## Plot 31-year average
p_31 = ggplot() + 
  geom_path(data = plot.data, aes(x = New_year, y = value, color = name),linewidth = 0.5) +
  scale_color_manual(values = colors_temp2[c(1,2)],
                     breaks = c("HR.SD.median","Tropical.T_Rec"),
                     labels = c("CGR Variance","Synthesized tropical temperature")) +
  scale_x_continuous(
    breaks = seq(1100, 2000, 50),
    labels = c("1100"," ","1200"," ","1300"," ","1400"," ","1500"," ","1600"," ","1700"," ","1800"," ","1900"," ","2000"),
    expand = c(0, 0),
    name = "Year",
    sec.axis = sec_axis(transform =  ~ . + 0, breaks = seq(1000, 2020, 100))
  ) +
  scale_y_continuous(
    name = "Z-score",
    breaks = seq(-2,3,0.5),
    sec.axis = sec_axis(transform =  ~ ., name = "Temperature Anomaly (°C)",breaks = c(-1,-0.5,0,0.5))
  ) +
  coord_cartesian(xlim = c(1100,2020),ylim = c(-2,4),expand = 0)+
  theme_figure1 +
  theme(
    plot.margin = margin(10, 15, 10, 10, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.05, 0.95)
  )

## Saving
ggsave(
  plot = p_31,
  filename = "Figures/Analysis/Compare Tropical T 31 years.tiff",
  width = 15,
  height = 10,
  unit = "cm"
)

# End -------------------------------------------------------------
