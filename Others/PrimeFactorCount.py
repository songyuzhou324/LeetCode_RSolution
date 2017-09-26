# function to calculate the count of all prime factors for arbitrary integer n >= 2
def get_prime_factor_count(n, dict):
    cnt = 1
    
    for i in range(n)[1:n]:
        while(n != (i+1)):
            if(n%(i+1) != 0):
                break;
            n = int(n/(i+1))
            if(dict[n-1] != 0):
                print('speed up')
                cnt = cnt + dict[n-1]
                break;
            else:
                cnt = cnt + 1   
                
    return(cnt)

# main function to get count for all integer from 2 to N
def get_prime_factor_count_main(N):
    dict = [0]*N
    
    for i in range(N)[1:N]:
        dict[i] = get_prime_factor_count(i+1, dict)
        
    return(sum(dict[1:N]))

get_prime_factor_count_main(6)
