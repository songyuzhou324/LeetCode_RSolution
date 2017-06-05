#####################################################################################################
# leetcode 276: Paint Fence

#There is a fence with n posts, each post can be painted with one of the k colors.
#You have to paint all the posts such that no more than two adjacent fence posts have the same color.
#Return the total number of ways you can paint the fence.
#Note: n and k are non-negative integers.
#####################################################################################################

# key: nth post's color differs either with n-1 or n-2, or both (complementary of 3 with same color in a row)
# if differs from n-1, we don't care about n-2; if differs from n-2, we don't care about n-1
# thus, dp[n] = (k-1)*dp[n-1] + (k-1)*dp[n-2]

PaintFence <- function(n,k){
  dp <- rep(0,n)
  dp[1] <- k
  dp[2] <- k*k
  for(i in 3:n){
    dp[i] <- (dp[i-1] + dp[i-2])*(k-1) 
  }
  return(dp[n])
}

PaintFence(3,2)

