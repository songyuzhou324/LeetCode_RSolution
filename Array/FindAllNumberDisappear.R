#############################################################################################################################
## leet code 448: find all numbers disappeared in an array
# Given an array of integers where 1 ??? a[i] ??? n (n = size of array), some elements appear twice and others appear once.

# Find all the elements of [1, n] inclusive that do not appear in this array.

# Could you do it without extra space and in O(n) runtime? You may assume the returned list does not count as extra space.

#Example:
  
#  Input:
#  [4,3,2,7,8,2,3,1]

#Output:
#  [5,6]
##############################################################################################################################
a <- c(4,3,2,7,8,2,3,1)

# key: if x[i] != i & x[x[i]] != x[i], then swap. At most there will be n-1 swap. grab index with x[index] != index

FindDisappear <- function(x){
  n <- length(x)
  for(i in 1:n){
    while(x[i] != i && x[x[i]] != x[i]){
      temp <- x[x[i]]
      x[x[i]] <- x[i]
      x[i] <- temp
    }
  }
  return(which(x != 1:n))
}

FindDisappear(a)