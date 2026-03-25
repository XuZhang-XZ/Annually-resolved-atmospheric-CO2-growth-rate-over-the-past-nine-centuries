

# Functions -------------------------------------------------------

## This function is used for the nesting algorithm of random forests
## The structure of this function is very similar to the rec.PCA function

rec.RF = function(Predictors = predictors,
                  Target = whole.T,
                  ntree,
                  maxnodes,
                  nodesize) {
  
  ## Original Data
  sel.period = c(1:2020)
  Total.result=data.frame("year"=sel.period)
  Predictors=merge(Total.result,Predictors,by="year",all.x = TRUE)
  Target=merge(Total.result,Target,by="year",all.x = TRUE)
  
  Total.result$Obs=Target$Obs
  Total.result$Total.Obs=Target$Total.Obs
  Total.result$Total.Obs1=Target$Total.Obs1
  
  ## Fifty years nesting
  Nesting.start.years=c(seq(1000,1900,by=50),2001,2004,2007)
  Nesting.end.years=c(seq(1049,1949,by=50),2003,2006,2010)
  Nesting.end.years[19] = 2000
  Ne.ge = 2
  Ne.ge = 15

  ## PCR regression
  for(Ne.ge in 1:length(Nesting.start.years)){
    
    ## Target year
    Target.years=c(Nesting.start.years[Ne.ge]:Nesting.end.years[Ne.ge])
    Target.rows = which(Total.result$year %in% Target.years)
    
    ## Select Non-NA predictors
    predictorss1 = Predictors[Target.rows,]
    sel.lies=which(complete.cases(t(predictorss1)))
    if(length(sel.lies)<=2){
      next
    } else {
      predictorss2 = Predictors[, sel.lies]
    }
    
    ## Sample 90% of predictors
    if(NCOL(predictorss2)<=5){
      samplelie = c(2:NCOL(predictorss2))
      predictorss2 = predictorss2[, c(1, samplelie)]
    } else {
      samplelie=sample(c(2:NCOL(predictorss2)),floor(0.9*(NCOL(predictorss2)-1)),replace = FALSE)
      predictorss2 = predictorss2[,c(1,samplelie)]
    }
    
    ## Data
    kk = NCOL(predictorss2) - 1
    names(predictorss2) = c("year",paste0("Pre_",c(1:kk)))
    whole1 = merge(predictorss2, Target[,c("year","Obs")])
    calibrate.row = which(complete.cases(whole1))
    
    # Create hyper parameter grid
    RFoptim = tuneRF(
      x = whole1[calibrate.row, paste0("Pre_", c(1:kk))],
      y = whole1[calibrate.row, "Obs"],
      stepFactor = 1.5,
      ntreeTry = ntree,
      maxnodes = maxnodes,
      nodesize = nodesize,
      plot = FALSE,
      trace = TRUE,
    )
    OOBe = RFoptim[,2]
    O.mtry = RFoptim[which.min(OOBe),1]
    
    ## Random Forest
    temp = randomForest(
      Obs ~ . - year,
      data = whole1[calibrate.row, ],
      mtry = O.mtry,
      ntree = ntree,
      maxnodes = maxnodes,
      nodesize = nodesize,
      corr.bias = TRUE
    )
    
    ## Prediction
    Pre.before = predict(temp, whole1)
    Pre.before[calibrate.row] = temp$predicted

    ## Select top 20 predictors with highest importance
    if(NCOL(whole1) > 21) {

      ## Select Predictors
      Sel.cols = order(temp$importance, decreasing = TRUE)[1:20]
      Sel.variables = row.names(temp$importance)[Sel.cols]

      ## Final
      whole2 = whole1[,c("year","Obs",Sel.variables)]
      calibrate.row = which(complete.cases(whole2))

      # Create hyper parameter grid
      RFoptim = tuneRF(
        x = whole2[calibrate.row, Sel.variables],
        y = whole2[calibrate.row, "Obs"],
        stepFactor = 1.5,
        ntreeTry = ntree,
        maxnodes = maxnodes,
        nodesize = nodesize,
        plot = FALSE,
        trace = TRUE,
      )
      OOBe = RFoptim[,2]
      O.mtry = RFoptim[which.min(OOBe),1]

      ## Random Forest
      temp = randomForest(
        Obs ~ . - year,
        data = whole2[calibrate.row, ],
        mtry = O.mtry,
        ntree = ntree,
        maxnodes = maxnodes,
        nodesize = nodesize,
        corr.bias = TRUE
      )

      ## Final
      Pre.before = predict(temp, whole2)
      Pre.before[calibrate.row] = temp$predicted

    }

    ## Test Variance
    if(1 == 0) {
      var(Pre.before[calibrate.row])
      var(temp$predicted)
      var(whole1$Obs[calibrate.row])
      plot(Total.result$year,Total.result$Obs,type = "l",xlim = c(1900,2020),ylim = c(-2,2))
      lines(Total.result$year,Pre.before,col = "red")
      lines(Total.result$year[calibrate.row],temp$predicted, col = "blue")
    }
    
    ## Output
    Target.RMSE = temp$mse[500] ^ 0.5
    Total.result[Target.rows, "RMSE"] = Target.RMSE
    Total.result[Target.rows, "R2"] = NSE(Target$Obs, Pre.before)
    Total.result[Target.rows, "R"] = cor(Target$Obs, Pre.before, use = "na.or.complete")
    Total.result[Target.rows, "Pre.before"] = Pre.before[Target.rows]
    
    ## Saving for particular years
    CGR.pre = Pre.before
    Total.result[,paste0("Pre.Values.",Nesting.start.years[Ne.ge])] = CGR.pre
    
    ## Add Residuals
    Add.errors = rnorm(n = length(CGR.pre),mean = 0, sd = Target.RMSE)
    CGR.pre.errors = CGR.pre + Add.errors
    Total.result[Target.rows,"Pre.errors"] = CGR.pre.errors[Target.rows]

    ## Adjust mean and SD
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
      All.S = data.frame("year" = Total.result$year,
                         "Total.Obs" = Total.result$Total.Obs,
                         "Obs" = Total.result$Obs,
                         "Pre.before" = Pre.before,
                         "Pre.errors" = CGR.pre.errors,
                         "Pre" = CGR.pre.errors.Var)
      All.S$Errors = All.S$Obs - All.S$Pre.before
      
      var(All.S$Total.Obs[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Obs[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Pre.before[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Errors[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      cor(All.S$Pre.before[which(All.S$year %in% c(1959:2000))],All.S$Errors[which(All.S$year %in% c(1959:2000))], use = "na.or.complete")
      
      var(All.S$Obs[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Pre.before[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Pre.errors[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      var(All.S$Pre[which(All.S$year %in% c(1959:2000))],na.rm = TRUE)
      
      var(Total.result$Pre[which(Total.result$year %in% c(1959:2000))])
      var(CGR.pre.errors.Var[which(Total.result$year %in% c(1959:2000))])
      var(Total.result$Pre.errors[which(Total.result$year %in% c(1959:2000))])
    }
    
    ## print
    print(Ne.ge)
    
  }
  
  return(Total.result)
}

# End ---------------------------------------------------------------------

