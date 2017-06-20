######################################################################################################################################
## leetcode 459: Repeated substring pattern

#Given a non-empty string check if it can be constructed by taking a substring of it and appending multiple copies of the substring together. 
#You may assume the given string consists of lowercase English letters only and its length will not exceed 10000.

#Example 1:
#  Input: "abab"
#Output: True
#Explanation: It's the substring "ab" twice.

#Example 2:
#Input: "aba"
#Output: False

#Example 3:
#Input: "abcabcabcabc"
#Output: True
#Explanation: It's the substring "abc" four times. (And the substring "abcabc" twice.)
#######################################################################################################################################

# idea: 1) if the string can be decomposed into multiple same substrings, then the length of the substring can't be greater than n/2
#       2) we start with length l <= n/2. if l is a factor of n, then we check if replicate string[1:l] = string

# solution:
RepeatedSubstringPattern <- function(inputstr){
  x <- unlist(strsplit(inputstr, split = ""))
  n <- length(x)
  res <- "False"
  if(n == 1){
    return("The only substring is itself")
  } else {
    for(l in round(n/2) : 1){
      if(n %% l == 0){
        times <- n/l
        test_string <- rep(x[1:l], times)
        if(paste(test_string, collapse = "") == inputstr){
          res <- "True"
          break;
        }
      }
    }
  }
  return(res)
}

a1 <- c("abab")
a2 <- c("aba")
a3 <- c("abcabcabcabc")
RepeatedSubstringPattern(a1)
RepeatedSubstringPattern(a2)
RepeatedSubstringPattern(a3)