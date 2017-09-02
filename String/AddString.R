<<<<<<< HEAD
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
=======
# leetcode 415: Add strings
# Given two non-negative integers num1 and num2 represented as string, return the sum of num1 and num2.

#Note:
#The length of both num1 and num2 is < 5100.
#Both num1 and num2 contains only digits 0-9.
#Both num1 and num2 does not contain any leading zero.
#You must not use any built-in BigInteger library or convert the inputs to integer directly.

# idea: a = '923', => ('9','2','3'), 
#       b = '77', => ('7','7') => ('0','7','7')
# manually check the number and get the result 

a <- '923'
b <- '77'

ref <- as.character(0:9)

AddStr <- function(x, y, ref){
  x_splt <- unlist(strsplit(x,""))
  y_splt <- unlist(strsplit(y,""))
  
  # padding
  x_len <- length(x_splt)
  y_len <- length(y_splt)
  if(x_len > y_len){
    y_splt <- c(as.character(rep(0,(x_len - y_len))),y_splt)
  } else if (y_len > x_len){
    x_splt <- c(as.character(rep(0,(y_len - x_len))),x_splt)
  }
  
  # calculation
  res <- rep("0", x_len)
  carry <- 0
  for(i in x_len:1){
    num_x <- which(x_splt[i] == ref) - 1
    num_y <- which(y_splt[i] == ref) - 1
    sum_xy <- num_x + num_y + carry
    if(sum_xy >= 10){
      carry <- 1
      res[i] <- as.character(sum_xy - 10)
    } else {
      carry <- 0
      res[i] <- as.character(sum_xy)
    }
  }
  
  # end of the loop, if carry == 1, add 1
  if(carry == 1){
    res <- c("1", res)
  }
  
  return(paste(res, collapse = ""))
}

AddStr(a, b, ref)

c <- '335'
d <- '789'

AddStr(c,d,ref)





>>>>>>> cee063689aa832b0399bed42766e6a2044d9d54b
