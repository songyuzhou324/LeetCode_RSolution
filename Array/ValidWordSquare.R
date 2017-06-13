###########################################
# leetcode 422: valid word square
#Given a sequence of words, check whether it forms a valid word square.
#A sequence of words forms a valid word square if the kth row and column read the exact same string, where 0 ??? k < max(numRows, numColumns).

#Note:The number of words given is at least 1 and does not exceed 500.
#Word length will be at least 1 and does not exceed 500.
#Each word contains only lowercase English alphabet a-z.
#Example 1:
  
#  Input:
#  [
#    "abcd",
#    "bnrt",
#    "crmy",
#    "dtye"
#    ]

#Output:
#  true

#Explanation:
#  The first row and first column both read "abcd".
#The second row and second column both read "bnrt".
#The third row and third column both read "crmy".
#The fourth row and fourth column both read "dtye".

#Therefore, it is a valid word square.
#Example 2:
  
#  Input:
#  [
#    "abcd",
#    "bnrt",
#    "crm",
#    "dt"
#    ]

#Output:
#  true

#Explanation:
#  The first row and first column both read "abcd".
#The second row and second column both read "bnrt".
#The third row and third column both read "crm".
#The fourth row and fourth column both read "dt".

#Therefore, it is a valid word square.
#Example 3:
#  
#  Input:
#  [
#    "ball",
#    "area",
#    "read",
#    "lady"
#    ]

#Output:
#  false

#Explanation:
#  The third row reads "read" while the third column reads "lead".
#Therefore, it is NOT a valid word square.
###########################################################################################################

# solution: restore in data frame and check all
a1 <- c("abcd",
        "bnrt",
        "crmy",
        "dtye")
a2 <- c("abcd",
        "bnrt",
        "crm",
        "dt")
a3 <- c("ball",
        "area",
        "read",
        "lady")
a4 <- c("abcd")
a5 <- c("a")
ValidWordSquare <- function(x){
  res <- "yes"
  char_list <- strsplit(x, split="")
  if(length(char_list) == 1){
    if(length(char_list[[1]]) != 1){
      res <- "no"
    }
  } else {
    for(i in 1:length(char_list)){
      for(j in 2:length(char_list[[i]])){
        if(char_list[[i]][j] != char_list[[j]][i]){
          res <- "no"
          break;
        }
      }
      if(res == "no"){
        break;
      }
    }
  }
  
  return(res)
}

ValidWordSquare(a1)
ValidWordSquare(a2)
ValidWordSquare(a3)
ValidWordSquare(a4)
ValidWordSquare(a5)