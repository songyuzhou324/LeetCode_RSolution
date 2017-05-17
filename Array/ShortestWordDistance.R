#########################################################################################################################
# leetcode 243: shortest word distance

# Given a list of words and two words word1 and word2, return the shortest distance between these two words in the list.
#For example, Assume that words = ["practice", "makes", "perfect", "coding", "makes"].
#Given word1 = "coding", word2 = "practice", return 3. Given word1 = "makes", word2 = "coding", return 1.
#Note: You may assume that word1 does not equal to word2, and word1 and word2 are both in the list
##########################################################################################################################

a <- c("practice", "makes", "perfect", "coding", "makes")

# solution: key: a pointer remember the index of last word should be check, keep updating the shortest distance
ShortestDist <- function(v,x,y){
  
  dist <- length(v) + 1
  indx <- -1
  for(i in 1:length(v)){
    #print(dist)
    if(v[i] %in% c(x,y)){
      indx <- i
      break
    }
  }
  for(i in (indx+1):length(v)){
    #print(dist)
    if(v[i] %in% c(x,y) & v[i] != v[indx]){
      dist <- min(dist, i-indx)
      indx <- i
    }
  }
  return(dist)
}

ShortestDist(a, x="coding", y = "practice")
ShortestDist(a, x="makes", y = "coding")