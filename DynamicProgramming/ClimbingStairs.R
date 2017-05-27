###################################################################################################
## leetcode 70: Climbing Staris

## You are climbing a stair case. It takes n steps to reach to the top.
##  Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
###################################################################################################

#### first of all, let's play with fibonacci sequence:
# 0,1,1,2,3,5,....
# f(n) = f(n-1) + f(n-2)
# how to find the f(n) when giving n?
# intuitively, we create a function recursively calling itselft:

recursive.fibonacci <- function(x){
  res <- 0
  if(x < 0){
    return( "value must be a non-negative integer")
  } else if (x == 0){
    return(0)
  } else if(x == 1 ){
    return(1)
  } else {
    res <- recursive.fibonacci(x-1) + recursive.fibonacci(x-2)
  }
  return(res)
}

recursive.fibonacci(6)

### However, it's very inefficient, for example, 
# f(6) = f(5) + f(4), f(5) = f(4) + f(3), f(4) already been calculated no need to calculate again!!
# the key of dynamic programming is, a recursive problem can be decomposed into sub-problems
# and we can save the result of sub-problems  so we no need to calculate it again!
# trade between space and time!

dynamic.fibonacci <- function(x){
  if(x < 0){
    return("input must be non-negative integer")
  } else if (x == 0){
    return(0)
  } else if (x == 1){
    return(1)
  } else {
    f <- rep(0, x+1)
    f[2] <- 1
    for(i in 3:(x+1)){
      f[i] <- f[i-1] + f[i-2]
    }
    return(f[x+1])
  }
}

dynamic.fibonacci(6)

## now back to climbing staris problem, for total n steps, if we know the distinct way to n-1 steps and n-2 steps
## the total number of distinct way to n steps = f(n-1) + f(n-2)
## this is because, from n-1 to n, only one choice, take 1 step; so f(n | last reached n-1 ) = f(n-1)
## from n-2 to n, if we take two separate one steps, it's totally the same as f(n-1); if take 1 step to n, it distincts from f(n-1)
## since we can only take 1 or 2 steps, f(n) = f(n | last reached n-1 ) + f(n | last reached n-2)
##                                           = f(n-1) + f(n-2)
## thus, a fibonacci sequence with f[1] = 1, f[2] = 2

ClimbStairs <- function(x){
  if(x < 0){
    return("input must be non-negative integer")
  } else if (x == 1){
    return(1)
  } else if (x == 2){
    return(2)
  } else {
    f <- rep(0, x)
    f[1] <- 1
    f[2] <- 2
    for(i in 3:x) {
      f[i] <- f[i-1] + f[i-2]
    }
    return(f[x])
  }
}

for(i in 1:5){
  print(ClimbStairs(i))
}