
## This function is used for calculating annual mean

Annual.mean = function(x,JJA.year) {
  x1 = NA
  for(i in 1:length(JJA.year)){
    ges = c((12*i-11):(12*i))
    x1[i] = mean(x[ges])
  }
  x1
}
