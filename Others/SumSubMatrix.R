#################################################################################
# calculate sum of a sub-matrix; the function could be called for many times
#################################################################################

# Assume the original big matrix is X
# preprocessing: generate another matrix Y, Y(i,j) = sum(X(0,0) : X(i,j))
# matrix Y can be generated in O(n) time with the recursive formula: Y(i,j) = Y(i-1,j) + Y(i, j-1) - Y(i-1,j-1) + X(i,j)
# finally, sum of arbitrary sub-matrix (start = X(i_0,j_0), end = X(i_1,j_1)) can be calculated by: Y(i_1,j_1) - Y(i_1,j_0) - Y(i_0,j_1) + Y(i_0,j_0)

# function for preprocessing:
Preprocess <- function(x){
  nr <- nrow(x)
  nc <- ncol(x)
  Y <- matrix(0,nr, nc)
  
  # initialize the first element
  Y[1,1] <- x[1,1]
  
  # initialize the first row and column of Y
  for(i in 2:nr){
    Y[i,1] <- Y[i-1,1] + x[i,1]
  }
  for(j in 2:nc){
    Y[1,j] <- Y[1,j-1] + x[1,j]
  }
  
  # use recursive formula to update the rest of Y
  for(i in 2:nr){
    for(j in 2:nc){
      Y[i,j] <- Y[i-1,j] + Y[i, j-1] - Y[i-1,j-1] + X[i,j]
    }
  }
  return(Y)
}

# main function to calculate the sum of arbitrary sub-matrix
SumSubMatrix <- function(y, start_i, start_j, end_i, end_j){
  if(start_i == 1 & start_j == 1){
    return(Y[end_i, end_j])
  } else if(start_i == 1 & start_j != 1){
    return(y[end_i,end_j] - y[end_i,start_j-1])
  } else if (start_i != 1 & start_j == 1){
    return(y[end_i,end_j] -  y[start_i-1,end_j])
  } else {
    return(y[end_i,end_j] - y[end_i,start_j-1] - y[start_i-1,end_j] + y[start_i-1,start_j-1])
  }
  
}




X <- matrix(sample(1:20, size = 25, replace = T), 5,5)

Y <- Preprocess(X)

SumSubMatrix(Y, 1,2,2,3)
SumSubMatrix(Y, 1,1,3,5)
SumSubMatrix(Y, 2,2,4,4)