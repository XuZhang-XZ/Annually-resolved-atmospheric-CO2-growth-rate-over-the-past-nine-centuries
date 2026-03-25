
# function ----------------------------------------------------------------

## Conduct 2d interpolation

# z <- array(1:16,c(4,4))
# x <- c(1,2,3,4)
# y <- c(1,2,3,4)
# xo <- c(1.1,2.1,3.1,4.1,5)
# yo <- c(1.1,2.1,3.1,4.1,5)
# library(rainfarmr)
# z0 <- remapcon(x, y, z, xo, yo)
# z1 <- conser(x, y, z, xo, yo)
# z2 <- conser_old(x, y, z, xo, yo)
# 
# z <- array(0,c(4,4))
# x <- c(1,2,3,4)
# y <- c(1,2,3,4)
# xo <- c(1.1,2.1,3.1,4.1)
# yo <- c(1.1,2.1,3.1,4.1)
# z1 <- conser(x, y, z, xo, yo)
# z2 <- conser_old(x, y, z, xo, yo)
# 
# z <- array(c(1:10,NA,NA,NA,NA,100,100),c(4,4))
# x <- c(1,2,3,4)
# y <- c(1,2,3,4)
# xo <- c(1.1,2.1,3.1,4.1)
# yo <- c(1.1,2.1,3.1,4.1)
# z1 <- conser(x, y, z, xo, yo)
# z2 <- conser_old(x, y, z, xo, yo)

conser=function (x, y, z, xo, yo) {
  nx <- length(x)
  nxo <- length(xo)
  xoe <- numeric(nxo + 2)
  xoe[2:(nxo + 1)] <- xo
  xoe[1] <- 2 * xo[1] - xo[2]
  xoe[nxo + 2] <- 2 * xo[nxo] - xo[nxo - 1]
  xol <- (xoe[2:(nxo + 2)] + xoe[1:(nxo + 1)])/2
  xo1 <- xol[1:nxo]
  xo2 <- xol[2:(nxo + 1)]
  xe <- numeric(nx + 2)
  xe[2:(nx + 1)] <- x
  xe[1] <- 2 * x[1] - x[2]
  xe[nx + 2] <- 2 * x[nx] - x[nx - 1]
  xl <- (xe[2:(nx + 2)] + xe[1:(nx + 1)])/2
  x1 <- xl[1:nx]
  x2 <- xl[2:(nx + 1)]
  
  ny <- length(y)
  nyo <- length(yo)
  yoe <- numeric(nyo + 2)
  yoe[2:(nyo + 1)] <- yo
  yoe[1] <- 2 * yo[1] - yo[2]
  yoe[nyo + 2] <- 2 * yo[nyo] - yo[nyo - 1]
  yol <- (yoe[2:(nyo + 2)] + yoe[1:(nyo + 1)])/2
  yo1 <- yol[1:nyo]
  yo2 <- yol[2:(nyo + 1)]
  ye <- numeric(ny + 2)
  ye[2:(ny + 1)] <- y
  ye[1] <- 2 * y[1] - y[2]
  ye[ny + 2] <- 2 * y[ny] - y[ny - 1]
  yl <- (ye[2:(ny + 2)] + ye[1:(ny + 1)])/2
  y1 <- yl[1:ny]
  y2 <- yl[2:(ny + 1)]
  
  wx <- matrix(0, nx, nxo)
  for (i in 1:nx) {
    for (j in 1:nxo) {
      d <- min(x2[i], xo2[j]) - max(x1[i], xo1[j])
      wx[i, j] <- d * (d > 0)
    }
  }
  wx <- wx/colSums(wx)[col(wx)]

  wy <- matrix(0, ny, nyo)
  for (i in 1:ny) {
    for (j in 1:nyo) {
      d <- min(y2[i], yo2[j]) - max(y1[i], yo1[j])
      wy[i, j] <- d * (d > 0)
    }
  }
  wy <- wy/colSums(wy)[col(wy)]
  
  ## Two dimension
  if(length(dim(z))==2){
    zo <- matrix(NA, nxo, nyo)
    for (i in 1:nxo) {
      for (j in 1:nyo) {
        ii <- which(wx[, i] != 0)
        jj <- which(wy[, j] != 0)
        weight=wx[ii, i] %*% t(wy[jj, j])
        
        ## Check out of range
        if(length(ii)==0|length(jj)==0){
          zo[i, j] <- NA
          next
        }
        
        ## Average
        zo[i, j] <- (sum(z[ii, jj] * weight, na.rm = TRUE)/sum(is.finite(z[ii,jj]) * weight))
        if(is.nan(zo[i, j])) {zo[i, j]=NA}
      }
    }
    return(zo)
  }
  
  ## Three dimension
  if(length(dim(z))==3){
    time.length=dim(z)[3]
    zo <- array(NA, c(nxo, nyo,time.length))
    for (i in 1:nxo) {
      for (j in 1:nyo) {
        ii <- which(wx[, i] != 0)
        jj <- which(wy[, j] != 0)
        weight=wx[ii, i] %*% t(wy[jj, j])
        
        ## Check out of range
        if(length(ii)==0|length(jj)==0){
          zo[i, j, ] <- NA
          next
        }
        
        ## Average
        for(tim in 1:time.length){
          zo[i, j, tim] <- (sum(z[ii, jj, tim] * weight, na.rm = TRUE)/sum(is.finite(z[ii,jj, tim]) * weight))
          if(is.nan(zo[i, j, tim])) {zo[i, j, tim]=NA}
          All.ppp = length(ii)*length(jj)
          if(length(which(is.finite(z[ii,jj, tim])))<(0.5*All.ppp)) {zo[i, j, tim]=NA}
        }
      }
      ## Print
      print(i/nxo)
    }
    return(zo)
  }
}




# z=matrix(NA,31,51)
# z=matrix(0,31,51)
# z=matrix(1,31,51)
# z=matrix(-1,31,51)
# 
# x <- seq(4, 10, 0.2)
# y <- seq(30, 40, 0.2)
# xo <- seq(5, 6, 0.5)
# yo <- seq(35, 37, 0.5)
# 
# z0 <- remapcon(x, y, z, xo, yo)
# z1 <- conser(x, y, z, xo, yo)
# z2 <- conser_old(x, y, z, xo, yo)



# old ---------------------------------------------------------------------

conser_old=function (x, y, z, xo, yo) {
  nx <- length(x)
  nxo <- length(xo)
  xoe <- numeric(nxo + 2)
  xoe[2:(nxo + 1)] <- xo
  xoe[1] <- 2 * xo[1] - xo[2]
  xoe[nxo + 2] <- 2 * xo[nxo] - xo[nxo - 1]
  xol <- (xoe[2:(nxo + 2)] + xoe[1:(nxo + 1)])/2
  xo1 <- xol[1:nxo]
  xo2 <- xol[2:(nxo + 1)]
  xe <- numeric(nx + 2)
  xe[2:(nx + 1)] <- x
  xe[1] <- 2 * x[1] - x[2]
  xe[nx + 2] <- 2 * x[nx] - x[nx - 1]
  xl <- (xe[2:(nx + 2)] + xe[1:(nx + 1)])/2
  x1 <- xl[1:nx]
  x2 <- xl[2:(nx + 1)]
  
  ny <- length(y)
  nyo <- length(yo)
  yoe <- numeric(nyo + 2)
  yoe[2:(nyo + 1)] <- yo
  yoe[1] <- 2 * yo[1] - yo[2]
  yoe[nyo + 2] <- 2 * yo[nyo] - yo[nyo - 1]
  yol <- (yoe[2:(nyo + 2)] + yoe[1:(nyo + 1)])/2
  yo1 <- yol[1:nyo]
  yo2 <- yol[2:(nyo + 1)]
  ye <- numeric(ny + 2)
  ye[2:(ny + 1)] <- y
  ye[1] <- 2 * y[1] - y[2]
  ye[ny + 2] <- 2 * y[ny] - y[ny - 1]
  yl <- (ye[2:(ny + 2)] + ye[1:(ny + 1)])/2
  y1 <- yl[1:ny]
  y2 <- yl[2:(ny + 1)]
  
  wx <- matrix(0, nx, nxo)
  for (i in 1:nx) {
    for (j in 1:nxo) {
      d <- min(x2[i], xo2[j]) - max(x1[i], xo1[j])
      wx[i, j] <- d * (d > 0)
    }
  }
  wx <- wx/colSums(wx)[col(wx)]
  
  wy <- matrix(0, ny, nyo)
  for (i in 1:ny) {
    for (j in 1:nyo) {
      d <- min(y2[i], yo2[j]) - max(y1[i], yo1[j])
      wy[i, j] <- d * (d > 0)
    }
  }
  wy <- wy/colSums(wy)[col(wy)]
  
  zo <- matrix(NA, nxo, nyo)
  for (i in 1:nxo) {
    for (j in 1:nyo) {
      ii <- which(wx[, i] != 0)
      jj <- which(wy[, j] != 0)

      ## Check out of range
      if(length(ii)==0|length(jj)==0){
        zo[i, j] <- NA
        next
      }
      
      ## Check NA values
      if(length(which(!is.na(z[ii, jj])))==0){
        zo[i, j] <- NA
        next
      }
      
      ww <- sum(z[ii, jj] * (wx[ii, i] %*% t(wy[jj, j])),na.rm = TRUE)
      Total.weight=sum(is.finite(z[ii,jj]) * (wx[ii, i] %*% t(wy[jj, j])))
      zo[i, j] <- ww/Total.weight
      
    }
  }
  return(zo)
}
