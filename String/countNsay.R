# leetcode 38: Count and say
#The count-and-say sequence is the sequence of integers with the first five terms as following:
  
#1.     1
#2.     11
#3.     21
#4.     1211
#5.     111221
#1 is read off as "one 1" or 11.
#11 is read off as "two 1s" or 21.
#21 is read off as "one 2, then one 1" or 1211.

countNsay <- function(n){
  # initial the result vector
  res <- c()
  
  # initial value when n == 1
  if(n == 1){
    res <- "1"
  } else if (n==2){
    res <- "11"
  } else {
    
    lst <- countNsay(n-1)
    lst_splt <- unlist(strsplit(lst,""))
    crrt <- lst_splt[1]
    cnt <- 1
    for(i in 2:length(lst_splt)){
      
      if(lst_splt[i] == crrt){
        cnt <- cnt + 1
      } else {
        res <- c(res, c(cnt, crrt))
        crrt <- lst_splt[i]
        cnt <- 1
        
      }
      
      if(i == length(lst_splt)){
        res <- c(res, c(cnt, crrt))
      }
    }
  }
  
  return(paste(res, collapse = ""))
}

countNsay(4)
countNsay(5)