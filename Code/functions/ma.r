
## Moving Average

ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}
