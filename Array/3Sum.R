#############################################################################################
## leetcode 15: single number
## Given an array S of n integers, are there elements a, b, c in S such that a + b + c = 0? 
## Find all unique triplets in the array which gives the sum of zero.
## For example, given array S = [-1, 0, 1, 2, -1, -4],
## A solution set is:
##  [
##    [-1, 0, 1],
##    [-1, -1, 2]
##  ]
#############################################################################################
a <- c(-1, 0, 1, 2, -1, -4)

# solution 1: brute force. time complexity: O(n^3)
three_sum_brute_force <- function(x){
  result <- data.frame()
  for(i in 1:(length(x)-2)){
    s2 <- 0 - x[i]
    for(j in (i+1):(length(x)-1)){
      s3 <- s2 - x[j]
      for(k in (j+1):length(x)){
        if(x[k] == s3){
          temp <- c(x[i], x[j], x[k])
          temp <- temp[order(temp)]
          result <- rbind(result, temp)
        }
      }
    }
  }
  result <- result[!duplicated(result),]
  return(result)
}

three_sum_brute_force(a)

# solution 2: hash table. time complexity: O(n^2)
three_sum_hash_table <- function(x){
   result <- data.frame()
   
   for(j in 1:(length(x)-1)){
     for(k in (j+1):length(x)){
       HashTable <- x[-c(j,k)]
       i <- which((0 - x[j] - x[k]) == HashTable)
       if(length(i) > 0){
         temp <- c(HashTable[i[1]], x[j], x[k])
         result <- rbind(result, temp[order(temp)])
       }
     }
   }
   result <- result[!duplicated(result),]
   return(result)
}

three_sum_hash_table(a)

# solution 3: two pointer. time complexity: O(n^2)
three_sum_two_pointer <- function(x){
  x_sort <- x[order(x)]
  result <- data.frame()
  n <- length(x)
  for(i in 1:(n-2)){
    start <- i+1
    end <- n
    while(start < end){
      if(x_sort[start] + x_sort[end] == -x_sort[i]){
        result <- rbind(result, c(x_sort[i],x_sort[start], x_sort[end]))
        break
      } else if( x_sort[start] + x_sort[end] > -x_sort[i]){
        end <- end - 1
      } else {
        start <- start + 1
      }
    }
  }
  return(result)
}

three_sum_two_pointer(a)