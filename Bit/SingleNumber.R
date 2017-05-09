###################################################################################################
# leetcode 136: single number
# Given an array of integers, every element appears twice except for one. Find that single one.
###################################################################################################
a <- c(1,2,1,3,4,5,4,5,2)

# solution 1: use table function in R to create contigency table 
single_number1 <- function(x){
  cnt_frame <- as.data.frame(table(x))
  return(cnt_frame[cnt_frame$Freq == 1,1])
}
single_number1(a)

# solution 2: use bitwise exlusive or operator
single_number2 <- function(x){
  result <- x[1]
  for(i in 2: (length(x))){
    print(result)
    result <- bitwXor(result,x[i])
    
  }
  return(result)
}
single_number2(a)

# final solution: use Reduce funtion to avoid for loop
single_number <- function(x){
  return(Reduce(bitwXor, x))
}
single_number(a)
