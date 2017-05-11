##############################################################################################################################
# leetcode 169: Majority Element

#Given an array of size n, find the majority element. The majority element is the element that appears more than ??? n/2 ??? times.
#You may assume that the array is non-empty and the majority element always exist in the array.
##############################################################################################################################
a <- c(9,1,2,9,2,3,9,4,9,9,9)
b <- c(2,2,2,2,3,4)
c <- c(1,2,2,3,4,5)
  
# solution: time complexity: at most O(n), but space ~O(n)
MajorityEle <- function(x){
  n <- length(x)
  cnt_table <- data.frame(value = x[1], cnt = 1)
  max_cnt <- 1
  for(i in 2:n){
    print(paste("iteration of ",i-1, sep=""))
    id <- which(x[i] %in% cnt_table$value)
    if(length(id) > 0){
      cnt_table$cnt[id] <- cnt_table$cnt[id] + 1
      max_cnt <- max(max_cnt, cnt_table$cnt[id])
    } else {
      cnt_table <- rbind(cnt_table, c(x[i], 1))
    }
    if(max_cnt > n/2){
      return(paste("The majority is ", cnt_table$value[id], sep=""))
    }
  }
  if(max_cnt <= n/2){
    return("no majority exits")
  }
}

MajorityEle(a)
MajorityEle(b)
MajorityEle(c)

# solutiuon 2: Boyer-Moore majority vote algorithm 
# time complexity O(n), space O(1)
# key idea: more increases for the majority number than decreases, so the counter will be positive at end with majority number stored.
# extreme case: [2,2,2,3,3,3,2,2]
MajorityEle_2 <- function(x){
  indx <- 1
  cnt <- 1
  for(i in 2:length(x)){
    if(x[i] == x[indx]){
      cnt <- cnt + 1
    } else {
      cnt <- cnt - 1
      if(cnt == 0){
        indx <- i
        cnt <- 1
      }
    }
  }
  return(x[indx])
}

MajorityEle_2(a)