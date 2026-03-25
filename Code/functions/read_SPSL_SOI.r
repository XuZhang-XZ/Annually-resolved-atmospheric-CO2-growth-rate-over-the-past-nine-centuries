
## Read data of SPSL format 

read_SPSL_SOI=function(file.name,Title1,NA.value = "-99.990"){
  
  ## read data
  NINO3434=read.table(file.name)
  NINO3434[which(NINO3434==(NA.value),arr.ind=TRUE)]=NA
  
  names(NINO3434)=c("year",c(1:12))
  NINO3434 = NINO3434[,1:13]
  
  ## Month Value
  NINO3434.month=pivot_longer(NINO3434,cols = c(2:13))
  names(NINO3434.month)=c("year","month","value")
  NINO3434.month$year=as.numeric(NINO3434.month$year)
  NINO3434.month$month=as.numeric(NINO3434.month$month)
  NINO3434.month=NINO3434.month[order(NINO3434.month$year,NINO3434.month$month),]
  NINO3434.month$Date=ymd(NINO3434.month$year*10000+NINO3434.month$month*100+1)

  ## Output
  Out.month=NINO3434.month[,c("Date","value")]
  names(Out.month)=c("Date",Title1)
  return(Out.month)
}


read_SOI=function(file.name,Title1){
  
  ## read data
  NINO3434=read.table(file.name)
  NINO3434[which(NINO3434==(-99.99),arr.ind=TRUE)]=NA
  names(NINO3434)=c("year",c(1:12))
  NINO3434 = NINO3434[,1:13]
  
  ## Month Value
  NINO3434.month=pivot_longer(NINO3434,cols = c(2:13))
  names(NINO3434.month)=c("year","month","value")
  NINO3434.month$year=as.numeric(NINO3434.month$year)
  NINO3434.month$month=as.numeric(NINO3434.month$month)
  NINO3434.month=NINO3434.month[order(NINO3434.month$year,NINO3434.month$month),]
  NINO3434.month$Date=ymd(NINO3434.month$year*10000+NINO3434.month$month*100+1)
  
  ## delete NA
  NINO3434.month=NINO3434.month[complete.cases(NINO3434.month),]
  
  ## Output
  Out.month=NINO3434.month[,c("Date","value")]
  names(Out.month)=c("Date",Title1)
  return(Out.month)
}

read_SPSL=function(file.name,Title1,missing.value = -99.99){
  
  ## read data
  NINO3434=read.table(file.name)
  NINO3434[which(NINO3434==(missing.value),arr.ind=TRUE)]=NA
  names(NINO3434)=c("year",c(1:12))
  
  ## Month Value
  NINO3434.month=pivot_longer(NINO3434,cols = c(2:13))
  names(NINO3434.month)=c("year","month","value")
  NINO3434.month$year=as.numeric(NINO3434.month$year)
  NINO3434.month$month=as.numeric(NINO3434.month$month)
  NINO3434.month=NINO3434.month[order(NINO3434.month$year,NINO3434.month$month),]
  NINO3434.month$Date=ymd(NINO3434.month$year*10000+NINO3434.month$month*100+1)
  
  ## delete NA
  NINO3434.month=NINO3434.month[complete.cases(NINO3434.month),]
  
  ## Output
  Out.month=NINO3434.month[,c("Date","value")]
  names(Out.month)=c("Date",Title1)
  return(Out.month)
}

