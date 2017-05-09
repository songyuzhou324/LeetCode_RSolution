#######################################################################################################
# leetcode 27: remove element
#Given an array and a value, remove all instances of that value in place and return the new length.

#Do not allocate extra space for another array, you must do this in place with constant memory.

#The order of elements can be changed. It doesn't matter what you leave beyond the new length.

#Example:
#Given input array nums = [3,2,2,3], val = 3

#Your function should return length = 2, with the first two elements of nums being 2.
########################################################################################################
a <- c(3,2,2,3)
v <- 3

# solution 1: need extra removement at last
RemoveElement_one <- function(x, value){
  id <- c()
  for(i in 1:length(x)){
    if(x[i] == value){
      id <- c(id, i)
    }
  }
  return(x[-id])
}

RemoveElement_one(a,v)

# solution 2: put the value not equal to value into a new vector
RemoveElement_two <- function(x, value){
  result <- c()
  for(i in 1:length(x)){
    if(x[i] != value){
      result <- c(result, x[i])
    }
  }
  return(result)
}

RemoveElement_two(a,v)

# solution 3: use R build-in function
RemoveElement_three <- function(x, value){
  id <- which(x != value)
  return(x[id])
}

RemoveElement_three(a,v)