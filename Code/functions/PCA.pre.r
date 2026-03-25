

# Predictions -----------------------------------------------------

## This function is used for principal components regression

PCR.pre = function(predictorss2 = predictorss2,
                   Target = Target,min.PCA=5,minpor=0.8) {
  
  
  ## PCA
  na.rows=complete.cases(predictorss2[,-1])
  predictorss2=predictorss2[na.rows,]
  
  mtcars.pca <-prcomp(predictorss2[,-1], center = TRUE,scale. = TRUE)
  Impca=summary(mtcars.pca)$importance
  
  ## Limit PCA number between 2 and min.PCA
  PCA.num = min(which(Impca[3, ] >= minpor & Impca[3, ] <= 1)[1], min.PCA)
  PCA.num = max(2,PCA.num)
  
  ## Predict PCA
  PCAss=predict(mtcars.pca,predictorss2[,-1])
  PCA.sel=data.frame(PCAss[,c(1:PCA.num)])
  
  ## Data for linear regression
  whole1=cbind(predictorss2[,1],PCA.sel)
  names(whole1)=c("year",paste0("PC",c(1:NCOL(PCA.sel))))
  whole1=merge(whole1,Target,all=TRUE)
  
  ## Check number of observations
  whole2=whole1[complete.cases(whole1),]
  if(NROW(whole2)<10) {
    return(list("Best.R2"=NA,"Best.sd"=NA,"Pre1"=NA))
  }
  
  ## best subset regression
  library(leaps)
  models <- regsubsets(Obs~.-year,data=whole1)
  Best.equation=get_model_formula1(models = models,outcome = "Obs")

  ## new whole
  kk1=lm(Best.equation, data=whole1)
  
  ## Pre
  Best.R2=summary(kk1)$r.squared
  Best.sd=sd(summary(kk1)$residuals)
  Pre1=predict(kk1,whole1)
  
  ## Return
  return(list("Best.R2"=Best.R2,"Best.sd"=Best.sd,"Pre1"=Pre1))
}

# End ---------------------------------------------------------------------
