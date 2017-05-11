###################################################################################################################################################################
# leetcode 121: Best time to buy and sell stock

#Say you have an array for which the ith element is the price of a given stock on day i.
#If you were only permitted to complete at most one transaction (ie, buy one and sell one share of the stock), design an algorithm to find the maximum profit.

# Example 1:
#  Input: [7, 1, 5, 3, 6, 4]
# Output: 5
#max. difference = 6-1 = 5 (not 7-1 = 6, as selling price needs to be larger than buying price)

#Example 2:
#  Input: [7, 6, 4, 3, 1]
#Output: 0
#
#In this case, no transaction is done, i.e. max profit = 0.
###################################################################################################################################################################
a1 <- c(7,1,5,3,6,4)
a2 <- c(7,6,5,4,3,1)

# solution 1: time complexity: O(n^2)
BestTimeStock <- function(x){
  earn <- 0
  for(i in 2:length(x)){
    earn <- max(earn, x[i] - x[1:(i-1)])
  }
  return(earn)
}

BestTimeStock(a1)
BestTimeStock(a2)

# solutioin 2: add a variable storing the minimum price. idea: max=max(max(...),.), min=min(min(...),.), time complexity: O(n)
BestTimeStock_2 <- function(x){
  earn <- 0
  min_price <- x[1]
  for(i in 2:length(x)){
    earn <- max(earn, x[i] - min_price)
    min_price <- min(min_price, x[i])
  }
  return(earn)
}
BestTimeStock_2(a1)
BestTimeStock_2(a2)





