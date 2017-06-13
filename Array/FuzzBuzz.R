#####################################################################################################
# leetcode 412: FuzzBuzz

#Write a program that outputs the string representation of numbers from 1 to n.
#But for multiples of three it should output "Fizz" instead of the number and for the multiples of five output "Buzz". For numbers which are multiples of both three and five output "FizzBuzz".
#Example:
  #n = 15,

#Return:
#  [
#    "1",
#    "2",
#    "Fizz",
#    "4",
#    "Buzz",
#    "Fizz",
#    "7",
#    "8",
#    "Fizz",
#    "Buzz",
#    "11",
#    "Fizz",
#    "13",
#    "14",
#    "FizzBuzz"
#    ]
##################################################################################################

# don't use %% operator

FuzzBuzz <- function(x){
  counter3 <- counter5 <- 0
  res <- rep(0,length(x))
  for(i in 1:length(x)){
    counter3 <- counter3 + 1
    counter5 <- counter5 + 1
    if(counter3 != 3 && counter5 != 5){
      res[i] <- paste(i,sep="")
    } else if(counter3 == 3 && counter5 !=5){
      res[i] <- "Fuzz"
      counter3 <- 0
    } else if(counter3 !=3 && counter5 == 5){
      res[i] <- "Buzz"
      counter5 <- 0
    } else {
      res[i] <- "FuzzBuzz"
      counter3 <- counter5 <- 0
    }
  }
  return(res)
}

FuzzBuzz(1:15)