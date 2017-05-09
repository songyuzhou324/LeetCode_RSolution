##################################################################
# leetcode 118: Pascal's Triangle

#Given numRows, generate the first numRows of Pascal's triangle.
#For example, given numRows = 5,
#Return
#[
#      [1],
#     [1,1],
#    [1,2,1],
#   [1,3,3,1],
#  [1,4,6,4,1]
#]
##################################################################

# solution 1: this is equal to choose(n, k), where n is the rownumber and n>2, k=2,3,...,n-1
PascalTriangle_binomial <- function(rownumber){
  if(rownumber == 1){
    return(c(1))
  } else if (rownumber == 2){
    return(c(1,1))
  } else {
    result <- rep(1,rownumber)
    n <- rownumber - 1
    for(i in 2:n) {
      result[i] <- choose(n,i-1)
    }
    return(result)
  }
}

PascalTriangle_binomial(5)

# solution 2: 
PascalTriangle <- function(rownumber){
  if(rownumber == 1){
    return(c(1))
  } else if (rownumber == 2){
    return(c(1,1))
  } else {
    prev <- c(1,1)
    curnt <- c()
    while(length(curnt) < rownumber){
      curnt <- rep(0,(length(prev)+1))
      n <- length(curnt)
      curnt[1] <- curnt[n] <- 1
      for(i in 2:(n -1)){
        curnt[i] <- prev[i-1] + prev[i]
      }
      prev <- curnt
    }
    return(curnt)
  }
}

PascalTriangle(5)



