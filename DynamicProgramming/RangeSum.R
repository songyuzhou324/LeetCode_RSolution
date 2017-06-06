#######################################################################################################
## leetcode 303: Range Sum Query - Immutable
#Given an integer array nums, find the sum of the elements between indices i and j (i ??? j), inclusive.
#Example:
#Given nums = [-2, 0, 3, -5, 2, -1]
#sumRange(0, 2) -> 1
#sumRange(2, 5) -> -1
#sumRange(0, 5) -> -3
########################################################################################################

a <- c(-2,0,3,-5,2,-1)

sumRange <- function(x, start, end){
  s <- x[start]
  for(i in (start+1):end){
    s <- s + x[i]
  }
  return(s)
}

sumRange(a,1,3)
sumRange(a,3,6)
sumRange(a,1,6)