# ########################################################################################################
# leetcode 198: house robber

# You are a professional robber planning to rob houses along a street. 
# Each house has a certain amount of money stashed, the only constraint stopping you 
# from robbing each of them is that adjacent houses have security system connected
# and it will automatically contact the police if two adjacent houses were broken into on the same night.
#
# Given a list of non-negative integers representing the amount of money of each house, 
# determine the maximum amount of money you can rob tonight without alerting the police.
##########################################################################################################

# solution: key: array dp, dp[i] for ith position, the maximum sum of non-adjacent number
# dp[i] = max(num[i] + dp[i-2], dp[i-1]): condition on whether rob num[i]
# try {3,2,1,5} as simple example

rob <- function(x){
  if(length(x) == 1){
    return(x)
  } 
  if(length(x) == 2){
    return(max(x))
  }
  if(length(x) >= 3){
    dp <- rep(0,length(x))
    dp[1] <- x[1]
    dp[2] <- max(x[1],x[2])
    for(i in 3:length(x)){
      dp[i] <- max(dp[i-1],dp[i-2] + x[i]) 
    }
    return(dp[length(dp)])
  }
}

rob(c(3,2,1,5))