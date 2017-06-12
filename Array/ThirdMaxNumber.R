########################################################################################
# leetcode 414: third maximum number
# Given a non-empty array of integers, return the third maximum number in this array. 
# If it does not exist, return the maximum number. 
# The time complexity must be in O(n).
########################################################################################

# my idea, make an array with length three, which stores the top three value. maintain it everytime
a1 <- c(3,2,1)
a2 <- c(1,2)
a3 <- c(2,2,3,1)

ThirdMax <- function(x){
  max1 <- max2 <- max3 <- NULL
  for(i in 1:length(x)){
    if(x[i] %in% c(max1, max2, max3)){
      next;
    } else if(x[i] > max1 || is.null(max1)){
      max3 <- max2
      max2 <- max1
      max1 <- x[i]
    } else if(x[i] > max2 || is.null(max2)){
      max3 <- max2
      max2 <- x[i]
    } else if(x[i] > max3 || is.null(max3)){
      max3 <- x[i]
    }
  }
  if(is.null(max3)){
    return(max1)
  } else {
    return(max3)
  }

}

ThirdMax(a1)
ThirdMax(a2)
ThirdMax(a3)