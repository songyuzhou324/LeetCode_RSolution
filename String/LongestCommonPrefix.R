# leetcode 14: longest common prefix
# example: {leets, leetcode, leet, leeds} = lee
# idea: horizontal scanning: LCP(S1,...,Sn) = LCP(LCP(LCP(S1,S2),S3),...,Sn)

s <- c("leets","leetcode","leet","leeds")

# function of finding longest common prefix of two words
lcp <- function(s1, s2){
  n <- min(length(s1), length(s2))
  
  indx <- 0
  for(i in 1:n){
    if(s1[i] == s2[i]){
      indx <- i
    } else {
      break
    }
  }
  
  if(indx == 0 ){
    warning("There is no common prefix!")
    return(c())
  } else {
    return(paste(s1[1:indx], collapse = ""))
  }
}

# test lcp function
lcp(tt[[1]],tt[[2]])
lcp("boat", "saya")

# main function to find the longest common prefix of all words in a string using horizontal scanning
LongestCommonPrefix <- function(s){
  # split the string for scanning
  s_split_list <- strsplit(s, "")
  
  comm_prefix <- c()
  for(i in 1:(length(s_split_list) -1)){
    comm_prefix <- lcp(s_split_list[[i]], s_split_list[[i+1]])
  }
  
  return(comm_prefix)
}

LongestCommonPrefix(s)