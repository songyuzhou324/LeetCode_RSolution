#################################################################################################################################################
## leet code 215: kth largest element in an array
## Find the kth largest element in an unsorted array. Note that it is the kth largest element in the sorted order, not the kth distinct element.

#For example,
#Given [3,2,1,5,6,4] and k = 2, return 5.
#Note: 
#  You may assume k is always valid, 1 ??? k ??? array's length.
#################################################################################################################################################

# solution: quick select algorithm. Complexity: on average O(n), worst O(n^2)

QuickSelect <- function(A, k){
  
  # randomly sample the pivot
  pivot <- A[sample(1:length(A), size = 1)]
  
  A1 <- A2 <- c()
  for(i in 1:length(A)){
    if(A[i] < pivot){
      A1 <- c(A1, A[i])
    } else if(A[i] > pivot){
      A2 <- c(A2, A[i])
    } else {
    }
  }
  if(k <= length(A2)){ # it's in the pile of big elements
    return(QuickSelect(A2, k))
  } else if(k > length(A) - length(A1)){ # it's in the pile of small elements
    return(QuickSelect(A1, k - (length(A) - length(A1))))
  } else { # it's the pivot
    return(pivot)
  }
  
}

a <- c(3,2,1,5,6,4)
QuickSelect(a, 2)

