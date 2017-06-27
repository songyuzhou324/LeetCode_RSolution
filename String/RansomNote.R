################################################################################################
## leet code 383: Ransom Note

#Given an arbitrary ransom note string and another string containing letters from all the magazines, 
#write a function that will return true if the ransom note can be constructed from the magazines ; otherwise, it will return false.
#Each letter in the magazine string can only be used once in your ransom note.

#Note:
#  You may assume that both strings contain only lowercase letters.

#canConstruct("a", "b") -> false
#canConstruct("aa", "ab") -> false
#canConstruct("aa", "aab") -> true
##################################################################################################

RansomNote <- function(x,y){
  x_char <- unlist(strsplit(x, split = ""))
  y_char <- unlist(strsplit(y, split = ""))
  res <- "false"
  for(i in 1:length(y_char)){
    id <- which(y_char[i] == x_char)[1]
    if(length(id) > 0){
      x_char <- x_char[-id]
      if(length(x_char) == 0){
        res <- "true"
        break;
      }
    }
  }
  return(res)
}

RansomNote("a","b")
RansomNote("aa","ab")
RansomNote("a","aab")

# hash table solution: create count table of each letter in magzine, then loop over ransom to compare with the hash table
RansomNote_Hash <- function(x,y){
  x_char <- unlist(strsplit(x, split = ""))
  y_char <- unlist(strsplit(y, split = ""))
  res <- "true"
  
  letter <- cnt <- c()
  # generate hash table for y
  # table function can simplify the following code
  for(i in 1:length(y_char)){
    id <- which(y_char[i] == letter)
    if(length(id) > 0){
      cnt[id] <- cnt[id] + 1
    } else {
      letter <- c(letter, y_char[i])
      cnt <- c(cnt, 1)
    }
  }
  
  # loop over x the compare with hash table
  for(i in 1:length(x_char)){
    id <- which(x_char[i] == letter)
    if(length(id) == 0){
      res <- "false"
      break;
    } else {
      cnt[id] <- cnt[id] - 1
      if(cnt[id] < 0){
        res <- "false"
        break;
      }
    }
  }
  
  return(res)
}

RansomNote_Hash("a","b")
RansomNote_Hash("aa","ab")
RansomNote_Hash("a","aab")