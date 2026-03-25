

# Functions -------------------------------------------------------

## Parameters
parallel.ge=1
minpor = 0.8

## Reconstruction Function
recon=function(parallel.ge){
  
  ## Library
  .libPaths(new = "Z:/Software/R/Library",include.site = TRUE)
  library(sinkr)
  functions.names=list.files(path = "Code/functions/")
  for(i in 1:length(functions.names)){
    source(paste0("Code/functions/",functions.names[i]))
  }
  
  ## Predictors
  predictors=read.csv("Output Data/Predictors/PAGS2k.csv")
  predictors[-1] = apply(predictors[-1],2, scale)
  
  ## Filling missing values
  as=as.matrix(predictors[which(predictors$year%in%c(1959:2000)),-1])
  as1 = dineof(Xo=as, n.max = 10, ref.pos = NULL, delta.rms = sd(as,na.rm=TRUE)/1000, method = "svds")
  predictors[which(predictors$year%in%c(1959:2000)),-1]=as1$Xa
  
  ## Target
  whole.T=read.csv("Output Data/CGR/CGR.csv")

  ## Explained Variance
  minpor = runif(1, min = 0.6, max = 0.8)
  
  ## This parameter represent the maximum number of PC we select. In rec.PCA, we limit the number of PC to a range of 2-5.
  ## Please find details in "rec.PCA" and "PCR.pre" function.
  minPCA = 5
  
  ## Reconstructions
  ## The sampling of proxy records is conducted in the rec.PCA function
  Total.result = rec.PCA(
    Predictors = predictors,
    Target = whole.T,
    min.PCA = minPCA,
    minpor = minpor
  )
  Write.name=paste0("Z:/2022/Science9_SM/Output Data/Reconstructed CGR/All Ensemble/",parallel.ge,".csv")
  write.csv(Total.result,Write.name,row.names = FALSE)
  
}

# Parallel --------------------------------------------------------

## Here we use parallel computation with 7 cores
# recon(1)
library(parallel)
detectCores(logical = F)
nparas = 1000
nge = c(1:nparas)
system.time({
  x <- nge
  cl <- makeCluster(7)
  results <- parLapply(cl, x, recon)
  stopCluster(cl)
})


# Test Total Results ----------------------------------------------

## Plot Time Series
NBP_Data_A = read.csv("Output Data/Reconstructed CGR/SD/NBP_Data_A.csv",row.names = 1)
soi_Yearly = read.csv("Output Data/Climate Anomaly/soi_Yearly.csv",row.names = 1)

## Total
Total = merge(Total.result,NBP_Data_A,all.x = TRUE)
Total = merge(Total,soi_Yearly,all.x = TRUE)

## Total Correlation
Total1 = Total[Total$year %in% c(1870:1958),]
cor.test(Total1$SOI,Total1$Pre.before)
Total1 = Total[Total$year %in% c(1910:1958),]
cor.test(Total1$NBP,Total1$Pre.before)
Total1 = Total[Total$year %in% c(1959:2000),]
cor.test(Total1$SOI,Total1$Pre.before)
cor.test(Total1$NBP,Total1$Pre.before)
cor.test(Total1$NBP,Total1$Total.Obs1,use = "na.or.complete")

## Plot Series
plot(Total.result$year,Total.result$Pre.before,type="l",ylim=c(-2,2),xlim = c(1901,2020))
lines(NBP_Data_A$year,-NBP_Data_A$NBP, col = "blue")
plot(Total.result$year,Total.result$Pre.before,type="l",ylim=c(-2,2),xlim = c(1870,2020))
lines(soi_Yearly$year,soi_Yearly$SOI_1, col = "blue")

## Plot Metrics
plot(Total.result$year,Total.result$CV.R2,type="l",ylim=c(-0.5,1))
lines(Total.result$year,Total.result$CV.R,col="green")
abline(h = 0)

## Plot Time Series
plot(Total.result$year,Total.result$Total.Obs,type="l",ylim=c(-2,2),xlim = c(1901,2020))
lines(Total.result$year,Total.result$Pre.before,col="red")
lines(Total.result$year,Total.result$Pre,col="blue")

## Reconstructed and Observed SD
sel.years.i = which(Total.result$year%in%c(1015:2006))
for(i in sel.years.i) {
  Total.result[i,"SD.CGR"]=var(Total.result$Pre[c((i-15):(i+15))])
  Total.result[i,"Obs.CGR.SD"]=var(Total.result$Total.Obs[c((i-15):(i+15))])
}

## Plot Variance
plot(Total.result$year,Total.result$SD.CGR, type = "l",xlim = c(1000,2020),ylim = c(0,0.4))
lines(Total.result$year,Total.result$Obs.CGR.SD, type = "l",col = "red")
abline(v = c(1500,1950,1959+15,1985,1991))
plot(Total.result$year,Total.result$SD.CGR, type = "l",xlim = c(1900,2020))
lines(Total.result$year,Total.result$Obs.CGR.SD, type = "l",col = "red")
abline(v = c(1950,1985,1991))

## Check Variance
var(Total.result$Pre[which(Total.result$year %in% c(1959:2000))])
var(Total.result$Total.Obs[which(Total.result$year %in% c(1959:2000))])

# Ensemble Median ---------------------------------------------------

## Data
Pre=data.frame(row.names=c(1:2020))
Pre.1000 = Pre.1200 = Pre.1400 = Pre.1600 = Pre.1800 = data.frame(row.names=c(1:2020))
Pre.before=data.frame(row.names=c(1:2020))
CV.values=data.frame(row.names=c(1:2020))
CRE = Cr = VCE = Vr = SCE = Sr = RMSE = data.frame(row.names = c(1:2020))
parallel.ge=1

## Read Ensemble Member
for(parallel.ge in 1:nparas){
  
  ## read
  Read.name=paste0("Z:/2022/Science9_SM/Output Data/Reconstructed CGR/All Ensemble/",parallel.ge,".csv")
  Total.result=read.csv(Read.name)
  
  ## Data
  Pre[,parallel.ge]=Total.result$Pre
  Pre.before[,parallel.ge]=Total.result$Pre.before
  CV.values[,parallel.ge]=Total.result$CV.Values.1900
  
  Pre.1000[,parallel.ge]=Total.result$Pre.Values.1000
  Pre.1200[,parallel.ge]=Total.result$Pre.Values.1200
  Pre.1400[,parallel.ge]=Total.result$Pre.Values.1400
  Pre.1600[,parallel.ge]=Total.result$Pre.Values.1600
  Pre.1800[,parallel.ge]=Total.result$Pre.Values.1800
  
  CRE[,parallel.ge]=Total.result$R2
  Cr[,parallel.ge]=Total.result$R
  VCE[,parallel.ge]=Total.result$CV.R2
  Vr[,parallel.ge]=Total.result$CV.R
  SCE[,parallel.ge]=Total.result$Split.R2
  Sr[,parallel.ge]=Total.result$Split.R
  RMSE[,parallel.ge]=Total.result$RMSE
}

## Test
var(Total.result$Pre[which(Total.result$year%in%c(1959:2000))])
var(Total.result$Total.Obs[which(Total.result$year%in%c(1959:2000))])

## Summary
Total.summary=data.frame("year"=Total.result$year,
                         "Obs"=Total.result$Obs,
                         "Total.Obs"=Total.result$Total.Obs,
                         "Total.Obs1"=Total.result$Total.Obs1)

Total.summary[,c("Pre.median")]=apply(Pre,1,median,na.rm=TRUE)
Total.summary[,c("Pre.0.025","Pre.0.975")]=t(apply(Pre,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("Pre.1000")]=apply(Pre.1000,1,median,na.rm=TRUE)
Total.summary[,c("Pre.1200")]=apply(Pre.1200,1,median,na.rm=TRUE)
Total.summary[,c("Pre.1400")]=apply(Pre.1400,1,median,na.rm=TRUE)
Total.summary[,c("Pre.1600")]=apply(Pre.1600,1,median,na.rm=TRUE)
Total.summary[,c("Pre.1800")]=apply(Pre.1800,1,median,na.rm=TRUE)

Total.summary[,c("CV.values.median")]=apply(CV.values,1,median,na.rm=TRUE)
Total.summary[,c("CV.values.0.025","CV.values.0.975")]=t(apply(CV.values,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("CRE.median")]=apply(CRE,1,median,na.rm=TRUE)
Total.summary[,c("CRE.0.025","CRE.0.975")]=t(apply(CRE,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("Cr.median")]=apply(Cr,1,median,na.rm=TRUE)
Total.summary[,c("Cr.0.025","Cr.0.975")]=t(apply(Cr,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("VCE.median")]=apply(VCE,1,median,na.rm=TRUE)
Total.summary[,c("VCE.0.025","VCE.0.975")]=t(apply(VCE,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("Vr.median")]=apply(Vr,1,median,na.rm=TRUE)
Total.summary[,c("Vr.0.025","Vr.0.975")]=t(apply(Vr,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("SCE.median")]=apply(SCE,1,median,na.rm=TRUE)
Total.summary[,c("SCE.0.025","SCE.0.975")]=t(apply(SCE,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("Sr.median")]=apply(Sr,1,median,na.rm=TRUE)
Total.summary[,c("Sr.0.025","Sr.0.975")]=t(apply(Sr,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("RMSE.median")]=apply(RMSE,1,median,na.rm=TRUE)
Total.summary[,c("RMSE.0.025","RMSE.0.975")]=t(apply(RMSE,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

## Write
write.csv(Total.summary,"Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv",row.names = FALSE)
write.csv(Pre,"Output Data/Reconstructed CGR/Ensemble Mean/All.ensemble.csv",row.names = FALSE)

# Test Total Summary ----------------------------------------------

## Read Data
NBP_Data_A = read.csv("Output Data/Reconstructed CGR/SD/NBP_Data_A.csv",row.names = 1)
soi_Yearly = read.csv("Output Data/Climate Anomaly/soi_Yearly.csv",row.names = 1)
Total.summary = read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")
Total.summary_O = read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary_O.csv")

## Plot Series
plot(Total.summary$year,Total.summary$Total.Obs,type = "l",xlim = c(1050,2020),ylim = c(-2,2))
lines(Total.summary$year,Total.summary$Pre.median,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.025,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.975,type = "l",col = "red")
lines(Total.summary_O$year,Total.summary_O$Pre.0.025,type = "l",col = "blue")
lines(Total.summary_O$year,Total.summary_O$Pre.0.975,type = "l",col = "blue")

## Total
Total = merge(Total.summary,NBP_Data_A,all.x = TRUE)
Total = merge(Total,soi_Yearly,all.x = TRUE)

## Plot NBP
plot(Total.result$year,Total.result$Pre,type="l",ylim=c(-2,2),xlim = c(1901,2020))
lines(NBP_Data_A$year,-NBP_Data_A$NBP, col = "blue")
plot(Total.result$year,Total.result$Pre,type="l",ylim=c(-2,2),xlim = c(1870,2020))
lines(Total$year,Total$SOI_1, col = "red")

## Total Correlation
Total1 = Total[Total$year %in% c(1870:1958),]
cor(Total1$SOI,Total1$Pre.median)
Total1 = Total[Total$year %in% c(1915:1958),]
cor(Total1$NBP,Total1$Pre.median)
Total1 = Total[Total$year %in% c(1959:2000),]
cor(Total1$SOI,Total1$Pre.median)
cor(Total1$NBP,Total1$Pre.median)
cor(Total1$NBP,Total1$Total.Obs1,use = "na.or.complete")

## Plot Series
plot(Total.summary$year,Total.summary$Total.Obs,type = "l",xlim = c(1950,2020),ylim = c(-0.8,1))
lines(Total.summary$year,Total.summary$Pre.median,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.025,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.975,type = "l",col = "red")
abline(v = c(1963,1982,1991,1992,1993))

## Plot Series
plot(Total.summary$year,Total.summary$Total.Obs,type = "l",xlim = c(1500,2020),ylim = c(-0.8,1))
lines(Total.summary$year,Total.summary$Pre.median,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.025,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Pre.0.975,type = "l",col = "red")

## Plot Evaluation Metrics
plot(Total.summary$year,Total.summary$VCE.median,type = "l",ylim = c(-0.5,1))
lines(Total.summary$year,Total.summary$VCE.0.025,type = "l")
lines(Total.summary$year,Total.summary$VCE.0.975,type = "l")
abline(h= 0)

## Plot RMSE
plot(Total.summary$year,Total.summary$RMSE.median,type = "l")
lines(Total.summary$year,Total.summary$RMSE.0.025,type = "l")
lines(Total.summary$year,Total.summary$RMSE.0.975,type = "l")

## Reconstructed SD
sel.years.i = which(Total.summary$year%in%c((1000+15):(2006-15)))
for(i in sel.years.i) {
  Total.summary[i,"SD.CGR"]=var(Total.summary$Pre.median[c((i-15):(i+15))],na.rm = T)
}
sel.years.i = which(Total.summary$year%in%c((1959+15):(2020-15)))
for(i in sel.years.i) {
  Total.summary[i,"Obs.CGR.SD"]=var(Total.summary$Total.Obs[c((i-15):(i+15))],na.rm = T)
}

## Plot Variance
par(mfcol = c(2,1))
plot(Total.summary$year,Total.summary$SD.CGR, type = "l",xlim = c(1000,2020),ylim = c(0,0.5))
lines(Total.summary$year,Total.summary$Obs.CGR.SD, type = "l",col = "red")
abline(h = Total.summary$SD.CGR[which(Total.summary$year == 1985)])
abline(v = c(1950,1985))
abline(h = Total.summary$SD.CGR[which(Total.summary$year == 1985)])
plot(Total.summary$year,Total.summary$SD.CGR, type = "l",xlim = c(1900,2020),ylim = c(0.05,0.5))
lines(Total.summary$year,Total.summary$Obs.CGR.SD, type = "l",col = "red")
abline(v = c(1950,1985,1991))

# Calculate Historical CGR Variance --------------------------------------------------------------

## Data
Total.summary=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")
All.ensemble=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/All.ensemble.csv")

## Test
var(Total.summary$Pre.median[which(Total.summary$year%in%c(1959:2000))])
var(Total.summary$Total.Obs[which(Total.summary$year%in%c(1959:2000))])
var(All.ensemble$V1[which(Total.summary$year%in%c(1959:2000))])
sd(Total.summary$Total.Obs[which(Total.summary$year%in%c(1959:2000))])

## Observed Variance
sel.years.i = which(Total.summary$year%in%c((1959+15):(2020-15)))
for(i in sel.years.i) {
  Total.summary[i,"Obs.CGR.Var"]=var(Total.summary$Total.Obs[c((i-15):(i+15))])
}

## Reconstructed Variance for ensemble memebers
sel.years.i = which(Total.summary$year%in%c((1100+15):(2006-15)))
Analysis.CGR=data.frame("year"=Total.summary$year)
for(en.ge in 1:nparas){
  for(i in sel.years.i) {
    Analysis.CGR[i,en.ge+1]=var(All.ensemble[c((i-15):(i+15)),en.ge])
  }
  print(en.ge)
}

## Summary
Total.summary$HR.SD.median=apply(Analysis.CGR[,-1],1,quantile,probs=0.5,na.rm=TRUE)
Total.summary$HR.SD.0.025=apply(Analysis.CGR[,-1],1,quantile,probs=0.025,na.rm=TRUE)
Total.summary$HR.SD.0.975=apply(Analysis.CGR[,-1],1,quantile,probs=0.975,na.rm=TRUE)

## Write
write.csv(Total.summary,"Output Data/Reconstructed CGR/SD/Total.summary.sd.csv",row.names = F)

## Test
plot(Total.summary$year,Total.summary$HR.SD.median,type = "l",xlim = c(1000,2000),ylim = c(0,0.45))
lines(Total.summary$year,Total.summary$HR.SD.0.025,type = "l",col = "red")
lines(Total.summary$year,Total.summary$HR.SD.0.975,type = "l",col = "red")
lines(Total.summary$year,Total.summary$Obs.CGR.Var,type = "l",col = "blue")
abline(h = Total.summary$HR.SD.median[Total.summary$year == 1985])
abline(v = 1500)

## Test
plot(Total.summary$year,Total.summary$HR.SD.median,type = "l",xlim = c(1800,2000),ylim = c(0,0.5))
lines(Total.summary$year,Total.summary$Obs.CGR.Var,type = "l",col = "blue")

# End -------------------------------------------------------------


