

# Climate Data ------------------------------------------------------------

## SOI Data
soi_data = read_SPSL_SOI(file.name = "Input Data/SOI index/soi_3dp.dat.txt",Title1 = "SOI",NA.value = "-99.990")
soi_data$year = year(soi_data$Date)
soi_data$month = month(soi_data$Date)

## From previous July to current June
soi_data$year[soi_data$month%in%c(7:12)] = soi_data$year[soi_data$month%in%c(7:12)]+1
soi_data = soi_data[soi_data$year%in%c(1867:2020),]
soi_Yearly = aggregate(cbind(SOI) ~ year, data = soi_data,mean)
soi_Yearly$SOI = pass.filter.NA.mean(soi_Yearly$SOI,W = 10)

## Adjustment for two y axes that will be plotted in Fig. 1d
soi_Yearly$SOI_1 = -(soi_Yearly$SOI-0)*0.5

## NINO 3.4
NINO34=read_SPSL(file.name = "Input Data/NINO 3.4/NINO 3.4 PSL.txt", Title1 = "NINO34")
NINO34$year = year(NINO34$Date)
NINO34$month = month(NINO34$Date)

## From previous July to current June
NINO34$year[which(NINO34$month%in%c(7:12))]=NINO34$year[which(NINO34$month%in%c(7:12))]+1
Annual.NINO34=aggregate(NINO34~year,data=NINO34,mean,na.rm=TRUE)
Annual.NINO34=Annual.NINO34[-which(Annual.NINO34$year==1870|Annual.NINO34$year==2021),]
Annual.NINO34$NINO34.10=pass.filter.NA(a=Annual.NINO34$NINO34,W=10,type="high",method = c("Butterworth"))
Annual.NINO34$NINO34.10_1 = Annual.NINO34$NINO34.10

## Write
write.csv(soi_Yearly,"Output Data/Climate Anomaly/soi_Yearly.csv")
write.csv(Annual.NINO34,"Output Data/Climate Anomaly/Annual.NINO34.csv")

# Data ------------------------------------------------------------

## CGR Reconstruction
Total.summary=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary1 = Total.summary[Total.summary$year%in%c(1959:2000),]

## Climate Index
NBP_Data_A = read.csv("Output Data/Reconstructed CGR/SD/NBP_Data_A.csv",row.names = 1)
soi_Yearly = read.csv("Output Data/Climate Anomaly/soi_Yearly.csv",row.names = 1)
Annual.NINO34 = read.csv("Output Data/Climate Anomaly/Annual.NINO34.csv",row.names = 1)

## Var
sd(Total.summary1$Pre.median)
sd(Total.summary1$Total.Obs)

## Total
Total = merge(Total.summary,NBP_Data_A,by="year",all.x =T)
Total = merge(Total,soi_Yearly,by="year",all.x =T)
Total = merge(Total,Annual.NINO34,by="year",all.x =T)

## Correlations with  NBP
Total1 = Total[Total$year %in% c(1959:1990,1993:2006),]
cor.test(Total1$NBP,Total1$Total.Obs1)
cor.test(Total1$NBP,Total1$Pre.median)
Total1 = Total[Total$year %in% c(1915:1958),]
cor.test(Total1$NBP,Total1$Pre.median)

## Correlations with  SOI
Total1 = Total[Total$year %in% c(1959:1990,1993:2006),]
cor.test(Total1$SOI,Total1$Total.Obs1)
cor.test(Total1$SOI,Total1$Pre.median)
Total1 = Total[Total$year %in% c(1867:1958),]
cor.test(Total1$SOI,Total1$Pre.median)

## Correlations
Cor.data = data.frame()
Time.periods = list(c(1959:1990,1993:2006),c(1915:1958),c(1867:1958))
i = 1

## Correlations
for(i in 1:length(Time.periods)) {
  Sel_time = Time.periods[[i]]
  Cor.data_1 = data.frame("Period" = paste0(Sel_time[1],"_",Sel_time[length(Sel_time)]))
  Total1 = Total %>% dplyr::filter(year %in% Sel_time)
  Cor.data_1[1,c("Obs_DGVM_Cor","Obs_DGVM_Cor_p")] = cor25.1(Total1$Total.Obs1,Total1$NBP)
  Cor.data_1[1,c("Obs_SOI","Obs_SOI_p")] = cor25.1(Total1$Total.Obs1,Total1$SOI)
  Cor.data_1[1,c("Obs_NINO34_Cor","Obs_NINO34_Cor_p")] = cor25.1(Total1$Total.Obs1,Total1$NINO34.10)
  Cor.data_1[1,c("Rec_DGVM_Cor","Rec_DGVM_Cor_p")] = cor25.1(Total1$Pre.median,Total1$NBP)
  Cor.data_1[1,c("Rec_SOI","Rec_SOI_p")] = cor25.1(Total1$Pre.median,Total1$SOI)
  Cor.data_1[1,c("Rec_NINO34_Cor","Rec_NINO34_Cor_p")] = cor25.1(Total1$Pre.median,Total1$NINO34.10)
  Cor.data = rbind(Cor.data,Cor.data_1)
}

## Print Results
Cor.data

## Correlations extending back in nest in 1000
i = 1
Cor.summary = data.frame()
for(i in 1:5){
  Start.year = c(1000,1200,1400,1600,1800)[i]
  Total1=Total[Total$year%in%c(1915:1990,1993:2000),]
  
  temp2 = cor.test(Total1[,paste0("Pre.",Start.year)],Total1$SOI)
  Cor.summary[i,c("Cor.SOI","Cor.SOI.p")] = c(temp2$estimate,temp2$p.value)
  
  temp2 = cor.test(Total1[,paste0("Pre.",Start.year)],Total1$NBP)
  Cor.summary[i,c("Cor.DGVM","Cor.DGVM.p")] = c(temp2$estimate,temp2$p.value)
}

# Filter for plotting
Total = Total %>% dplyr::filter(year %in% c(1867:2006))
Total$NBP[which(!Total$year %in% c(1915:2006))] = NA

## Write
write.csv(Total,"Output Data/Reconstructed CGR/Plot.Total.csv")

# End -------------------------------------------------------------
