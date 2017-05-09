##############################################################################################################################
# LeetCode 26: Remove Duplicated from Sorted Array
# Given a sorted array, remove the duplicates in place such that each element appear only once and return the new length.

# Do not allocate extra space for another array, you must do this in place with constant memory.

# For example, Given input array nums = [1,1,2],

# Your function should return length = 2, with the first two elements of nums being 1 and 2 respectively. 
# It doesn't matter what you leave beyond the new length.
#############################################################################################################################
a <- c(1,1,2)
RemoveDuplicates <- function(x){
  result <- x[1]
  for(i in 2:length(x)){
    if(x[i] != result[length(result)]){
      result <- c(result, x[i])
    }
  }
  return(result)
}

RemoveDuplicates(a)

b <- c(1,1,2,3,4,4,4,4,5,6,7,7,8,9,9)
RemoveDuplicates(b)


