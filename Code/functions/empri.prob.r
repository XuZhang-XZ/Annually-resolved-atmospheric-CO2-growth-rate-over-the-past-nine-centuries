
## Calculate empirical probability

empri.prob = function(x,x1) {
  prob = length(which(x>=x1))/length(x)
  prob
}
