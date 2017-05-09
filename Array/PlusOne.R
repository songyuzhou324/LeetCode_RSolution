######################################################################################################
## leetcode 66: plus one

# Given a non-negative integer represented as a non-empty array of digits, plus one to the integer.
# You may assume the integer do not contain any leading zero, except the number 0 itself.
# The digits are stored such that the most significant digit is at the head of the list.
######################################################################################################

a1 <- c(5,4,6,7,9,9)
a2 <- c(9,9,9,9,9,9)

PlusOne <- function(x){
  n <- length(x)
  for(i in n:1){
    if(x[i] < 9){
      x[i] <- x[i] + 1
      break
    } else {
      x[i] <- 0
    }
  }
  if(x[1] == 0){
    x <- c(1,x)
  }
  return(x)
}

PlusOne(a1)
PlusOne(a2)