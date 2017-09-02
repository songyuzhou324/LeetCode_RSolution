#leetcode 58; legnth of last word
#Given a string s consists of upper/lower-case alphabets and empty space characters ' ', return the length of last word in the string.
#If the last word does not exist, return 0.
#Note: A word is defined as a character sequence consists of non-space characters only.
#For example, 
#Given s = "Hello World",
#return 5.

# a detector detecting space, a counter and a variable store length of latest word
s <- "Hello this stupid world"

LengthLastWord <- function(x){
  x_splt <- unlist(strsplit(x, ""))
  if(length(x_splt)==0){
    stop("string must be non-empty")
  }
  
  cnt <- 0
  ll <- 0
  for(i in 1:length(x_splt)){
    if(x_splt[i] != " "){
      cnt <- cnt + 1
      if(i == length(x_splt)){
        ll <- cnt
      }
    } else {
      ll <- cnt
      cnt <- 0
    }
  }

  return(ll)
}

LengthLastWord(s)