
## Function to get the best subset regression

get_model_formula1 <- function(models, outcome="CGR"){
  
  ## Summary Model
  res.sum=summary(models)
  id.hang = which.max(res.sum$adjr2)
  sell=which(res.sum[["which"]][id.hang,])
  sell=sell[-1]
  sell.names=names(sell)
  
  ## Predictors
  pres <- paste(sell.names, collapse = "+")
  
  # Build model formula
  return(as.formula(paste0(outcome, "~", pres)))
}


get_model_formula <- function(id, object, outcome){
  
  # get models data
  models <- summary(object)$which[id,-1]
  
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}




