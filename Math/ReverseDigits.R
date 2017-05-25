#######################################
## leetcode 7: reverse digits
# Reverse digits of an integer.

#Example1: x = 123, return 321
#Example2: x = -123, return -321
#######################################

# solution
# hint: 123 % 10 = 3 => rev = 3*100,trunc(123 / 10) = 12, repeat

ReverseDigit <- function(x){
  rev <- 0
  n <- nchar(x)
  s <- sign(x)
  x <- abs(x)
  if(n == 1){
    return(x)
  } else {
    for(i in 1:n){
      last_digit <- x %% 10
      rev <- rev + last_digit * 10^(i-1)
      x <- trunc(x/10)
    }
  }
  return(s*rev)
}

ReverseDigit(-123)

## actually there is a closed form which can achieve O(1) in computational time