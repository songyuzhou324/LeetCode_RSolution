#############################################################################################################
# leetcode 53: Maximum subarry
#
# Find the contiguous subarray within an array (containing at least one number) which has the largest sum.

# For example, given the array [-2,1,-3,4,-1,2,1,-5,4],
# the contiguous subarray [4,-1,2,1] has the largest sum = 6
#############################################################################################################
a <- c(-2,1,-3,4,-1,2,1,-5,4)

# solution 1:
# brute force: search all contiguous subarray and find the max
MaxSubarray_bf <- function(x){
  
  # initialize the max with the 1 number maximum
  
  maxx <- max(x)
  max_id <- which(x == maxx)
  
  for(i in 1:(length(x) - 1)){
    for(j in (i+1):length(x)){
      temp <- sum(x[i:j])
      if(temp > maxx){
        maxx <- temp
        max_id <- c(i, j)
      }
    }
  }
  return(max_id)
}

MaxSubarray_bf(a)

## However, we can save a lot: for example, we calculate x[1]+x[2]+x[3], later calculate x[2]+x[3] again. if store x[2]+x[3], no need to recaulate this part
## use a matrix M to store the result: M(i,j) = sum(x[i:j])
## thus, M(i,i) = x[i], we have the recursive formula: M(i,j) = M(i, j-1) + M(j,j), for i < j
# O(n^2)
MaxSubarray_dp <- function(x){
  n <- length(x)
  M <- matrix(0, n, n)
  maxx <- -999999999

  for(i in 1:(n-1)){
    for(j in (i+1) : n){
   
        M[i,j] <- M[i,j-1] + x[j] # x[j] = M[j,j]
        maxx <- max(maxx, M[i,j], x[j])
    }
  }
  return(maxx)
}
MaxSubarray_dp(a)


# O(n) solution
MaxSubarray_dp2 <- function(x){
  summ <- rep(0,length(x))
  maxx <- summ[1] <- x[1]
  for(i in 2:length(x)){
    summ[i] <- max(summ[i-1] + x[i], x[i])
    maxx <- max(maxx, summ[i])
  }
  return(maxx)
}

MaxSubarray_dp2(a)