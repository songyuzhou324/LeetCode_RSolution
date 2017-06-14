#####################################################
# leetcode 293: Flip Game
#You are playing the following Flip Game with your friend: Given a string that contains only these two characters: + and -, you and your friend take turns to flip two consecutive "++" into "--". The game ends when a person can no longer make a move and therefore the other person will be the winner.
#Write a function to compute all possible states of the string after one valid move.

#For example, given s = "++++", after one move, it may become one of the following states:
  
#  [
#    "--++",
#    "+--+",
#    "++--"
#    ]
#####################################################

a1 <- c("+","+","+","+")
a2 <- c("+","-","+","+")

# solution:

FlipGame <- function(x){
  count <- 0
  n <- length(x)
  if(n <= 1){
    return(count)
  } else {
    for(i in 2:n){
      if(x[i] == x[i-1] && x[i-1] == "+"){
        count <- count + 1
      }
    }
  }
  return(count)
}

FlipGame(a1)
FlipGame(a2)
