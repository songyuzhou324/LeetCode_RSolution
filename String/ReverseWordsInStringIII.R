#################################################################################################################################
## leetcode 557: Reverse words in a string III

#Given a string, you need to reverse the order of characters in each word within a sentence while still preserving whitespace and initial word order.
#Example 1:
#  Input: "Let's take LeetCode contest"
#Output: "s'teL ekat edoCteeL tsetnoc"
####################################################################################################################################
a <- "Let's take LeetCode contest"

# solution:
reverseWord <- function(x){
  n <- length(x)
  if(n <= 1){
    return(x)
  } else {
    start <- 1
    end <- n
    while(start < end){
      temp <- x[start]
      x[start] <- x[end]
      x[end] <- temp
      start <- start + 1
      end <- end - 1
    }
  }
  return(x)
}

# reverseWord(c("t","a","k","e"))

ReverseString <- function(x){
  words <- unlist(strsplit(x, split = " "))
  for(i in 1:length(words)){
    reverse_char <- reverseWord(unlist(strsplit(words[i], split = "")))
    words[i] <- paste(reverse_char, collapse = '')
  }
  return(paste(words, collapse = " "))
}

ReverseString(a)


# avoid splitting by space, scan onece with O(n)
# key: start from beginning, use two index, i, j. j start from i, when j encounters space, start reversing, then, let i=j, then j start from i again

ReverseStringII <- function(x){
  chars <- unlist(strsplit(x, split = "")) # break into characters
  n <- length(chars)
  res <- c()
  i <- 1
  
    for(j in (i+1):n){
      if(j == n){
        res <- c(res, paste(reverseWord(chars[i:j]), collapse = ""))
        break;
      } else if (chars[j] == " "){
        res <- c(res, paste(reverseWord(chars[i:(j-1)]), collapse = ""))
        i <- j+1
      }
    }
  return(paste(res, collapse = " "))
}

ReverseStringII(a)