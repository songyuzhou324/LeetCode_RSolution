###########################################################################################################################
# leetcode 434: number of segments in string

# Count the number of segments in a string, where a segment is defined to be a contiguous sequence of non-space characters.
#Please note that the string does not contain any non-printable characters.

#Example:
  
#  Input: "Hello, my name is John"
#Output: 5
############################################################################################################################

# solution:
a <- c("Hello, my name is John")

NumberOfSegments <- function(inputstr){
  x <- unlist(strsplit(inputstr, split = ""))
  count <- 1
  for(i in 1:length(x)){
    if(x[i] == " "){
      count <- count + 1
    }
  }
  return(count)
}

NumberOfSegments(a)