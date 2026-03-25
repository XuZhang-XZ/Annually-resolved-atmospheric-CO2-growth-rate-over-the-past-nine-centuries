


# Functions -------------------------------------------------------

## This function is used for the nesting algorithm of principal components regression

rec.PCA=function(Predictors=predictors,
                 Target=whole.T,
                 min.PCA=min.PCA,
                 minpor=minpor) {
  
  ## Read Original Data
  sel.period = c(1:2020)
  Total.result=data.frame("year"=sel.period)
  Predictors=merge(Total.result,Predictors,by="year",all.x = TRUE)
  Target=merge(Total.result,Target,by="year",all.x = TRUE)
  
  Total.result$Obs=Target$Obs
  Total.result$Total.Obs=Target$Total.Obs
  Total.result$Total.Obs1=Target$Total.Obs1
  
  ## Nests grouped by fifty years
  Nesting.start.years=c(seq(1000,1900,by=50),2001,2004,2007)
  Nesting.end.years=c(seq(1049,1949,by=50),2003,2006,2010)
  Nesting.end.years[19] = 2000
  Ne.ge = 1
  
  ## PCR regression for each nests
  for(Ne.ge in 1:length(Nesting.start.years)){
    
    ## Target year
    Target.years=c(Nesting.start.years[Ne.ge]:Nesting.end.years[Ne.ge])
    Target.rows=which(Total.result$year%in%Target.years)
    
    ## Select Non-NA predictors
    predictorss1 = Predictors[Target.rows,]
    sel.lies=which(complete.cases(t(predictorss1)))
    if(length(sel.lies)<=2){
      next
    } else {
      predictorss2 = Predictors[, sel.lies]
    }
    
    ## Sample 90% of predictors. 
    ## The condition with less than 5 predictors is not used in this article, because we have over 10 proxy records in 1100 CE
    if(NCOL(predictorss2)<=5){
      samplelie = c(2:NCOL(predictorss2))
      predictorss2 = predictorss2[, c(1, samplelie)]
    } else {
      samplelie=sample(c(2:NCOL(predictorss2)),floor(0.9*(NCOL(predictorss2)-1)),replace = FALSE)
      predictorss2 = predictorss2[,c(1,samplelie)]
    }
    
    ## PCR This use the PCR.pre function to conduct PCR
    temp= PCR.pre(
      predictorss2 = predictorss2,
      Target = Target[,c("year","Obs")],
      min.PCA=min.PCA,
      minpor=minpor
    )
    Total.result[Target.rows, "R2"] = temp$Best.R2
    Total.result[Target.rows, "R"] = cor(Target$Obs, temp$Pre1, use = "na.or.complete")
    Total.result[Target.rows, "RMSE"] = Target.RMSE = RMSE(Target$Obs, temp$Pre1)
    Total.result[Target.rows, "Pre.before"] = temp$Pre1[Target.rows]
    
    ## Saving for particular years
    CGR.pre = temp$Pre1
    Total.result[,paste0("Pre.Values.",Nesting.start.years[Ne.ge])] = temp$Pre1
    
    ## Add Errors
    Add.errors = rnorm(n = length(CGR.pre),mean = 0, sd = Target.RMSE)
    CGR.pre.errors = CGR.pre + Add.errors
    Total.result[Target.rows,"Pre.errors"] = CGR.pre.errors[Target.rows]
    
    ## Adjust mean and SD to observations
    Adjust.period = c(1959:2000)
    a1 = CGR.pre.errors[which(Total.result$year %in% Adjust.period)]
    a1.mean = mean(a1)
    a1.sd = sd(a1)
    a2 = Total.result$Total.Obs[which(Total.result$year %in% Adjust.period)]
    a2.mean = mean(a2)
    a2.sd = sd(a2)
    CGR.pre.errors.Var = (CGR.pre.errors - a1.mean) / a1.sd * a2.sd + a2.mean
    Total.result[Target.rows,"Pre"] = CGR.pre.errors.Var[Target.rows]
    Total.result[Target.rows,"Change.sd1"] = a2.sd / a1.sd
    Total.result[Target.rows,"Change.sd2"] = (a2.sd^2 / (a2.sd^2 - Target.RMSE^2))^0.5
    
    ## Test Variance
    if (1 == 0) {
      var(Total.result$Total.Obs[which(Total.result$year %in% c(1959:2000))])
      var(Total.result$Pre[which(Total.result$year %in% c(1959:2000))])
      var(CGR.pre.errors.Var[which(Total.result$year %in% c(1959:2000))])
      var(Total.result$Pre.errors[which(Total.result$year %in% c(1959:2000))])
    }
    
    ## Leave-5-years-out cross-validation
    start.years=c(1959,1964,1969,1974,1979,1984,1989,1993,1997)
    end.years = c(1963,1968,1973,1978,1983,1988,1992,1996,2000)
    CV.values=array(NA,NROW(Target))
    j=2
    for(j in 1:length(start.years)){
      
      Vali.years = c(start.years[j]:end.years[j])
      Cali.years = c(1959:2000)[which(!c(1959:2000)%in%Vali.years)]
      Cali.Target=Target
      Cali.Target$Obs[which(!Cali.Target$year%in%Cali.years)]=NA
      
      temp= PCR.pre(
        predictorss2 = predictorss2,
        Target = Cali.Target[,c("year","Obs")],
        min.PCA=min.PCA,
        minpor=minpor
      )
      CV.values[which(Target$year%in%Vali.years)]=temp$Pre1[which(Target$year%in%Vali.years)]
    }
    Total.result[Target.rows,"CV.R2"]=NSE(obs=Target$Obs,pre = CV.values)
    Total.result[Target.rows,"CV.R"]=cor(Target$Obs,CV.values,use="na.or.complete")
    Total.result[Target.rows,"CV.RMSE"]=RMSE(Target$Obs,CV.values)
    
    ## Saving for particular years
    Total.result[,paste0("CV.Values.",Nesting.start.years[Ne.ge])] = CV.values

    ##  Split-sample validation
    Cali.years = c(1972:2000)
    Vali.years = c(1959:1971)
    Cali.Target=Target
    Cali.Target$Obs[which(!Cali.Target$year%in%Cali.years)]=NA
    temp= PCR.pre(
      predictorss2 = predictorss2,
      Target = Cali.Target[,c("year","Obs")],
      min.PCA=min.PCA,
      minpor=minpor
    )
    CV.values=array(NA,NROW(Target))
    CV.values[which(Target$year%in%Vali.years)]=temp$Pre1[which(Target$year%in%Vali.years)]
    
    Total.result[Target.rows,"Split.R2"]=NSE(obs=Target$Obs,pre = CV.values)
    Total.result[Target.rows,"Split.R"]=cor(Target$Obs,CV.values,use="na.or.complete")
    Total.result[Target.rows,"Split.RMSE"]=RMSE(Target$Obs,CV.values)
    
    ## print
    print(Ne.ge)
    
  }
  
  ## Plot
  if(1 == 0) {
    plot(Total.result$year,Total.result$Obs,type = "l",xlim = c(1900,2020),ylim = c(-2,2))
    lines(Total.result$year,Total.result$Pre,col = "red")
    plot(Total.result$year,Total.result$Obs,type = "l",xlim = c(1000,2020),ylim = c(-2,2))
    lines(Total.result$year,Total.result$Pre,col = "red")
    
    plot(Total.result$year,Total.result$Change.sd1^2,type = "l",xlim = c(1000,2020),ylim = c(-2,4))
    plot(Total.result$year,Total.result$Change.sd2^2,type = "l",xlim = c(1000,2020),ylim = c(-2,4))
    
  }
  
  return(Total.result)
}

# End ---------------------------------------------------------------------
