# leetcode 28: Implement strStr()/ sub-string match
# Returns the index of the first occurrence of needle in haystack, or -1 if needle is not part of haystack.
# optimal solution: KMP algorithm:
# check the youtube video here: https://www.youtube.com/watch?v=kBW6oPaVjq0
#   1. compute prefix table which will tell us the length of the longest sub-string that match with the prefix of the pattern.
#   2. use the prefix table to do KMP mathcing

# function to compute prefix table 
# example: 
#q:      1, 2, 3, 4, 5, 6, 7
#P[q]:   a, b, c, d, a, b, d
#pi[q]:  0, 0, 0, 0, 1, 2, 0
getPrefixTable <- function(P){
  
  m <- length(P)
  
  # the prefix table with the same length of P
  pi <- rep(0, m)
  
  # pi[1] = 0 since no letter (prefix) before it
  pi[1] <- 0
  
  # initial k
  k <- 0
  
  for(q in 2: m){ # scan the pattern P[2:m] from left to right 
    
    while(k >0 & P[k+1] != P[q]){
      k <- pi[k] # the prefix P[k] is not the suffix of P[q]
    }
    
    if(P[k+1] == P[q]){
      k <- k+1  # the longest prerfix P[k] is also a proper suffix of P[q]
    }
    
    # update prefix table
    pi[q] <- k 
  }
  
  return(pi)
}

getPrefixTable(c("a","b","c","d","a","b","d"))

# function to do KMP mathcing
# key: if mismatch is detected, we shift to the right q-pi[q] steps
KMP <- function(T,P){
  n <- length(T)
  m <- length(P)
  pi <- getPrefixTable(P) # compoute prefix table
  
  q <- 0 # number of characters matched
  for(i in 1:n){
    while(q > 0 & P[q+1] != T[i]){ # when mismatch occured
      q <- pi[q] # shift the pattern q-pi[q] to the right, which means current number of matching equal to pi[q].  
                 # this means the first pi[q] number of characters in pattern P no longer need to be checked
                 # we start checking T[i] with the q+1 element in pattern P until we find a new match and break out the while
    }
    
    if(P[q+1] == T[i]){
      q <- q + 1
    }
    
    if(q == m){ # all of pattern P[1:m] matched, pattern founded
      print(paste("Pattern occurs at", i-m+1, "to", i))
      break # break the loop since only the first match is required
    }
      
  }
}

T <- unlist(strsplit('abcdab abcdabcdabde',""))
P <- unlist(strsplit('abcdabd',""))

KMP(T,P)
