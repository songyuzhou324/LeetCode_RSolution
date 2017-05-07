##############################################################################################################
# 2 Sum:
# Given an array of integers, return indices of the two numbers such that they add up to a specific target.
# You may assume that each input would have exactly one solution, and you may not use the same element twice.
# Example:
#  Given nums = [2, 7, 11, 15], target = 9,
# Because nums[0] + nums[1] = 2 + 7 = 9,
# return [0, 1].
###############################################################################################################
a <- c(2,7,11,15)
t <- 9
# solution 1: brute force; time complexity: O(n^2), space complexity: O(1)
two_sum_brute_force <- function(x, target){
  result <- data.frame()
  for(i in 1:(length(x)-1)){
    for(j in (i+1):length(x)){
      if(x[i] + x[j] == target){
        result <- rbind(result, c(i,j))
      }
    }
  }
  return(result)
}
two_sum_brute_force(a, t)

# solution 2: two pointers; only applied for sorted array; time complexity: O(n) 
two_sum_two_poiter <- function(x, target){
  result <- data.frame()
  i <- 1
  j <- length(x)
  while(i < j){
    ss <- x[i] + x[j]
    if(ss == target){
      result <- rbind(result, c(i, j))
      break;
    } else if (ss < target){
      i <- i + 1
    } else {
      j <- j - 1
    }
  }
  return(result)
}

two_sum_two_poiter(a, t)

# solution 3: Hash map: time complexity: O(n), space complexity: O(n)
# hash table: key = array value, value = index
two_sum_hash_table <- function(x, target){
  result <- data.frame()
  HashTable <- data.frame(key = x[1], index = 1)
 
  for(i in 2:length(x)){
    print(i)
    y <- target - x[i]
    
    # look up hash table 
    loc <- which(HashTable[,1] == y)
    if(length(loc) > 0){
      result <- rbind(result, c(HashTable[loc,2], i))
    }
    
    # save current value to hash table
    HashTable <- rbind(HashTable, c(x[i],i))
  }
  
  return(result)
  
}

two_sum_hash_table(a, t)
