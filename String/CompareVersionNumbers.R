# leetcode 165: compare version numbers
# Compare two version numbers version1 and version2.
#If version1 > version2 return 1, if version1 < version2 return -1, otherwise return 0.
#You may assume that the version strings are non-empty and contain only digits and the . character.
#The . character does not represent a decimal point and is used to separate number sequences.
#For instance, 2.5 is not "two and a half" or "half way to version three", it is the fifth second-level revision of the second first-level revision.
#Here is an example of version numbers ordering: 0.1 < 1.1 < 1.2 < 13.37

a <- 1.5
b <- 4.1
c <- 4.13

CompareVersionNumbers <- function(v1,v2){
  v1_splt <- as.numeric(unlist(strsplit(as.character(v1), "[.]")))
  v2_splt <- as.numeric(unlist(strsplit(as.character(v2), "[.]")))
  
  res <- 0
  if(v1_splt[1] > v2_splt[1]){
    res <- 1
  } else if (v1_splt[1] < v2_splt[1]){
    res <- -1
  } else if(v1_splt[2] > v2_splt[2]){
    res <- 1
  } else if(v1_splt[2] < v2_splt[2]){
    res <- -1
  }
  
  return(res)
}

CompareVersionNumbers(a,b)
CompareVersionNumbers(b,c)
CompareVersionNumbers(c,a)