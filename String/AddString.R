# leetcode 67: add binary
#Given two binary strings, return their sum (also a binary string).
#For example,
#a = "11"
#b = "1"
#Return "100".

AddBinary <- function(x,y){
  x_splt <- as.numeric(unlist(strsplit(x,"")))
  y_splt <- as.numeric(unlist(strsplit(y,"")))
  
  # padding
  lx <- length(x_splt)
  ly <- length(y_splt)
  if(lx == 0 || ly == 0){
    stop("string length must be positive integer")
  } else if (lx > ly){
    y_splt <- c(rep(0,lx-ly), y_splt)
  } else if (ly > lx){
    x_splt <- c(rep(0,ly-lx), x_splt)
  }
  
  # calculation
  res <- rep(0,max(lx,ly))
  carry <- 0
  for(i in max(lx,ly):1){
    sm <- x_splt[i]+y_splt[i]+carry
    if(sm>1){
      res[i] <- sm - 2
      carry <- 1
    } else {
      res[i] <- sm
      carry <- 0
    }
  }
  
  if(carry == 1){
    res <- c(1,res)
  }
  
  return(res)
}

a <- "11"
b <- "1"
AddBinary(a,b)

c <- "11"
d <- "11"
AddBinary(c,d)