# leetcode 125: valid palindrome
# A palindrome is a word, phrase, number, or other sequence of characters which reads the same backward as forward, such as madam or racecar
# For example, "A man, a plan, a canal: Panama" is a palindrome. "race a car" is not a palindrome.

a <- 'A man, a plan, a canal: Panama'
b <- 'race a car'

IsPalindrome <- function(x){
  res <- 'True'
  x_splt <- unlist(strsplit(x, ""))
  
  # if empty string, it is True
  # for non -empty string
  if(length(x_splt) > 0){
    
    # if not letter, remove 
    x_splt <- x_splt[grepl("[[:alpha:]]", x_splt)]
    # all to lower case
    x_splt <- tolower(x_splt)
    
    # check palindrome using two pointers
    n <- length(x_splt)
    i <- 1
    j <- n
    while(i<j){
      if(x_splt[i] != x_splt[j]){
        res <- 'False'
        break;
      }
      i <- i+1
      j <- j-1
    }
  } 
    
  return(res) 
}

IsPalindrome(a)
IsPalindrome(b)