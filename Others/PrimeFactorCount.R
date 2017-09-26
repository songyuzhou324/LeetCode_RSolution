# function to calculate the count of all prime factors
# n >= 2
get_prime_factor_count <- function(n, dict){
  
  cnt <- 1
  
  for(i in 2:n){
    
     while(n != i){
       
       if(n%%i != 0 ){
         
         break;
       }
       
       n <- n/i
       
       if(dict[n] != 0){     # look up the dictionary table 
         
         print('speed up')
         
         cnt <- cnt + dict[n]
         
         break
         
       } else {
         
         cnt <- cnt + 1
         
       }
    }
  }

  return(cnt)
  
}


# main function to get count for all from (2, N)

get_prime_factor_count_main <- function(N){
  # create a vector to store the count we already had
  # the vector can be referred by later calculation 
  dict = rep(0, N)
  for(i in 2:N){
    dict[i] <- get_prime_factor_count(i, dict)
  }
  
  return(sum(dict[2:N]))
  
}

get_prime_factor_count_main(40)
