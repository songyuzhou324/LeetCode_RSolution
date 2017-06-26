###############################################################################################################################
## leet code 408: valid word abbreviation

#Given a non-empty string s and an abbreviation abbr, return whether the string matches with the given abbreviation.

#A string such as "word" contains only the following valid abbreviations:
  
#  ["word", "1ord", "w1rd", "wo1d", "wor1", "2rd", "w2d", "wo2", "1o1d", "1or1", "w1r1", "1o2", "2r1", "3d", "w3", "4"]
#Notice that only the above abbreviations are valid abbreviations of the string "word". Any other string is not a valid abbreviation of "word".

#Note:
#  Assume s contains only lowercase letters and abbr contains only lowercase letters and digits.

#Example 1:
  
#  Given s = "internationalization", abbr = "i12iz4n":
  
#  Return true.
################################################################################################################################

# solution: 
ValidWordAbb <- function(str_char, abb_char){
  str_pointer <- 0
  res <- "True"
  for(i in 1:length(abb_char)){
    options(warn=-1)
    if(is.na(as.numeric(abb_char[i]))){
      str_pointer <- str_pointer + 1
      if(str_char[str_pointer] != abb_char[i]){
        res <- "False"
        break;
      }
    } else {
      str_pointer <- str_pointer + as.numeric(abb_char[i])
    }
  }
  options(warn=-1)
  return(res)
}


s1 <- c("a","p","p","l","e")
a1 <- c("a","2","e")

s2 <- c("w", "o", "r", "d")
a2 <- c("1" ,"o", "1", "d")

s3 <- c("i","n","t","e","r","n","a","t","i","o","n","a","l","i","z","a","t","i","o","n")
a3 <- c("i","12","i","z","4","n")

ValidWordAbb(s1, a1)
ValidWordAbb(s2, a2)
ValidWordAbb(s3, a3)