##############################################################################################
# leetcode 189: rotate array

# Rotate an array of n elements to the right by k steps.
# For example, with n = 7 and k = 3, the array [1,2,3,4,5,6,7] is rotated to [5,6,7,1,2,3,4]
##############################################################################################

a <- 1:7

# solution: key: use reverse three times: 1) reverse all 2) reverse last k 3) reverse first n-k
# extra space O(1)

# reverse function
reverse <- function(x, start, end){
  while(start <  end){
    temp <- x[start]
    x[start] <- x[end]
    x[end] <- temp
    start <- start + 1
    end <- end - 1
  }
  return(x)
}

# rotate
rotate <- function(x,k){
  n <- length(x)
  x <- reverse(x,1,n) # reverse all
  x <- reverse(x,(n-k+1),n) # reverse last k elements
  x <- reverse(x, 1, (n-k)) # reverse first n-k elements
  return(x)
}

rotate(a, 3)


