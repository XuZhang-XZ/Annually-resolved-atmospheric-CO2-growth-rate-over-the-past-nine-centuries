

# Read Predictors for RF ------------------------------------------------------

## Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
predictors = data.frame("year" = c(1:2030))
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
  raw.chro$year = raw.chro$year + properties2[ge, "Max.Cor.year"]
  
  ## Combine
  predictors=merge(predictors,raw.chro,all.x = T)
}

# Percentage of missing values
as=as.matrix(predictors[which(predictors$year%in%c(1959:2000)),-1])
length(which(is.na(as)))/length(as) ## 0.07953394
table(properties2$archive)
table(properties2$seasonality)

## Write
write.csv(predictors,"Output Data/Predictors/PAGS2k_RF predictors.csv",row.names = FALSE)

# Functions -------------------------------------------------------

## Paras
parallel.ge=1

## Recon
recon = function(parallel.ge) {
  
  ## Library
  .libPaths(new = "Z:/Software/R/Library",include.site = TRUE)
  library(randomForest)
  library(sinkr)
  functions.names=list.files(path = "Code/functions/")
  for(i in 1:length(functions.names)){
    source(paste0("Code/functions/",functions.names[i]))
  }
  
  ## Predictors
  predictors=read.csv("Output Data/Predictors/PAGS2k_RF predictors.csv")
  predictors[-1] = apply(predictors[-1],2, scale)
  
  ## Filling missing values
  as=as.matrix(predictors[which(predictors$year%in%c(1959:2000)),-1])
  as1 = dineof(Xo=as, n.max = 10, ref.pos = NULL, delta.rms = sd(as,na.rm=TRUE)/1000, method = "svds")
  predictors[which(predictors$year%in%c(1959:2000)),-1]=as1$Xa
  
  ## Target
  whole.T = read.csv("Output Data/CGR/CGR.csv")
  
  ## Parameters
  ntree = sample(500:2000,1)
  nodesize =  sample(5:10,1)
  maxnodes = sample(5:20,1)

  ## Reconstructions
  ## The sampling of proxy records is conducted in the functions/rec.RF.r function
  Total.result = rec.RF(
    Predictors = predictors,
    Target = whole.T,
    ntree = ntree,
    maxnodes = maxnodes,
    nodesize = nodesize
  )
  Write.name = paste0("Z:/2022/Science9_SM/Output Data/Reconstructed CGR/All Ensemble_RF/",parallel.ge,".csv")
  write.csv(Total.result,Write.name,row.names = FALSE)
  
}

# Parallel --------------------------------------------------------

# Parameters
library(parallel)
detectCores(logical = F)
nparas = 1000
nge = c(1:nparas)

## Parallel
system.time({
  x <- nge
  cl <- makeCluster(10)
  results <- parLapply(cl, x, recon)
  stopCluster(cl)
})

# Test For Ensemble Members ----------------------------------------------

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
plot(Total.result$year,Total.result$R2,type="l",ylim=c(-0.5,1))
lines(Total.result$year,Total.result$R,col="green")
plot(Total.result$year,Total.result$RMSE,type="l",ylim=c(0,1))
plot(Total.result$year,Total.result$RMSE ^ 2,type="l",ylim=c(0,0.4))
plot(Total.result$year,Total.result$Change.sd1,type="l",ylim=c(0.5,1.5))
abline(h = 1)

## Plot Time Series
plot(Total.result$year,Total.result$Total.Obs,type="l",ylim=c(-2,2),xlim = c(1901,2020))
lines(Total.result$year,Total.result$Pre.before,col="red")
lines(Total.result$year,Total.result$Pre,col="blue")

## Reconstructed and Observed SD
sel.years.i = which(Total.result$year%in%c(1015:2006))
for(i in sel.years.i) {
  Total.result[i,"SD.CGR"]=var(Total.result$Pre[c((i-15):(i+15))])
  Total.result[i,"SD.CGR.before"]=var(Total.result$Pre.before[c((i-15):(i+15))])
  Total.result[i,"SD.CGR.errors"]=var(Total.result$Pre.errors[c((i-15):(i+15))])
  Total.result[i,"Obs.CGR.SD"]=var(Total.result$Total.Obs[c((i-15):(i+15))])
}

## Plot Variance
plot(Total.result$year,Total.result$SD.CGR, type = "l",xlim = c(1000,2020),ylim = c(0,0.4))
lines(Total.result$year,Total.result$Obs.CGR.SD, type = "l",col = "red")
lines(Total.result$year,Total.result$SD.CGR.before, type = "l",col = "red")
lines(Total.result$year,Total.result$SD.CGR.errors, type = "l",col = "blue")
abline(v = c(1500,1950,1959+15,1985,1991))
plot(Total.result$year,Total.result$SD.CGR, type = "l",xlim = c(1900,2020))
lines(Total.result$year,Total.result$Obs.CGR.SD, type = "l",col = "red")
abline(v = c(1950,1985,1991))

## Check Variance
var(Total.result$Pre[which(Total.result$year %in% c(1959:2000))])
var(Total.result$Total.Obs[which(Total.result$year %in% c(1959:2000))])

# Ensemble Mean ---------------------------------------------------

## Read Data
Pre=data.frame(row.names=c(1:2020))
Pre.1000 = Pre.1200 = Pre.1400 = Pre.1600 = Pre.1800 = data.frame(row.names=c(1:2020))
Pre.before=data.frame(row.names=c(1:2020))
CV.values=data.frame(row.names=c(1:2020))
CRE = Cr = VCE = Vr = SCE = Sr = RMSE = data.frame(row.names = c(1:2020))
parallel.ge=1

## Read Ensemble Member
for(parallel.ge in 1:nparas){
  
  ## read
  Read.name=paste0("Z:/2022/Science9_SM/Output Data/Reconstructed CGR/All Ensemble_RF/",parallel.ge,".csv")
  Total.result=read.csv(Read.name)
  
  ## Data
  Pre[,parallel.ge]=Total.result$Pre
  Pre.before[,parallel.ge]=Total.result$Pre.before
  
  ## Replace with Observation
  Sel.row = which(Total.result$year %in% c(1959:2020))
  Pre[Sel.row,parallel.ge]=Total.result$Total.Obs[Sel.row]

  ## Metrics
  CRE[,parallel.ge]=Total.result$R2
  Cr[,parallel.ge]=Total.result$R
  RMSE[,parallel.ge]=Total.result$RMSE
}

## Test Variancec
var(Total.result$Pre[which(Total.result$year%in%c(1959:2000))])
var(Total.result$Total.Obs[which(Total.result$year%in%c(1959:2000))])

## Summary
Total.summary=data.frame("year"=Total.result$year,
                         "Obs"=Total.result$Obs,
                         "Total.Obs"=Total.result$Total.Obs,
                         "Total.Obs1"=Total.result$Total.Obs1)

Total.summary[,c("Pre.median")]=apply(Pre,1,median,na.rm=TRUE)
Total.summary[,c("Pre.0.025","Pre.0.975")]=t(apply(Pre,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

Total.summary[,c("RMSE.median")]=apply(RMSE,1,median,na.rm=TRUE)
Total.summary[,c("RMSE.0.025","RMSE.0.975")]=t(apply(RMSE,1,quantile,probs=c(0.025,0.975),na.rm=TRUE))

## Observed Variance
sel.years.i = which(Total.summary$year%in%c((1959+15):(2020-15)))
for(i in sel.years.i) {
  Total.summary[i,"Obs.CGR.Var"]=var(Total.summary$Total.Obs[c((i-15):(i+15))])
}

## Reconstructed Variance
sel.years.i = which(Total.summary$year%in%c((1100+15):(2006-15)))
Analysis.CGR=data.frame("year"=Total.summary$year)
All.ensemble= Pre

## Variance
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
write.csv(Total.summary,"Output Data/Reconstructed CGR/SD/Total.summary.sd_RF.csv",row.names = F)

# Comparison --------------------------------------------------------------

## Data
Total.summary2 = Total.summary
Total.summary1 = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary2 = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_RF.csv")

## Plot
plot(Total.summary1$year,Total.summary1$HR.SD.median,type = "l",xlim = c(1000,2000),ylim = c(0,0.4))
lines(Total.summary1$year,Total.summary1$HR.SD.0.025,col = "black")
lines(Total.summary1$year,Total.summary1$HR.SD.0.975,col = "black")

lines(Total.summary2$year,Total.summary2$HR.SD.median,col = "red")
lines(Total.summary2$year,Total.summary2$HR.SD.0.025,col = "blue")
lines(Total.summary2$year,Total.summary2$HR.SD.0.975,col = "blue")

## Plot
plot(Total.summary1$year,Total.summary1$Pre.median,type = "l",xlim = c(1000,1200),ylim = c(-1,1))
lines(Total.summary2$year,Total.summary2$Pre.median,col = "red")

## Plot
plot(Total.summary1$year,Total.summary1$Pre.median,type = "l",xlim = c(1400,1600),ylim = c(-1,1))
lines(Total.summary2$year,Total.summary2$Pre.median,col = "red")

## Plot
plot(Total.summary1$year,Total.summary1$RMSE.median ^2 ,type = "l",xlim = c(1000,2000),ylim = c(0,0.45))
lines(Total.summary2$year,Total.summary2$RMSE.median ^ 2, col = "red")

# End ---------------------------------------------------------------------


