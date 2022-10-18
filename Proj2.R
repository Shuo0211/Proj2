#name...
#address of github repo: https://github.com/Shuo0211/Proj2.git
# contribution:....

strategies <- function(n,k,strategy,v1,v2){ 
  A <- cbind(v1,v2) #v1: number of boxes  v2:random number in boxes
  if (strategy == 1){ 
    #strategy1
    start <- A[k,1] #k-th prisoner opens the k-th box
    steps <- 1
    found <- start == A[k,2] # TURE for k-th prisoner found number k from the k-th box, otherwise False
    while (found == FALSE && steps<(2*n)) {
      k <- match(c(A[k,2]),v1) #find the next box labeled with the number found before
      found <- start == A[k,2] #If this drawer contains their number, they are done and were successful, otherwise continue
      steps <- steps + 1       #count steps
    }
    return(steps)
  }
  else if (strategy==2){
    #strategy2 
    start  <- sample(v1, 1, replace=FALSE)  #number of box selected randomly
    steps <- 1
    found <- A[start,2] == k #TURE for k-th prisoner found number k from the random selected box, otherwise False
    while (found == FALSE && steps<(2*n)) { 
      start <- match(c(A[start,2]),v1)  #find the next box labeled with the number found before
      found <- A[start,2] == k          #If this drawer contains their number, they are done and were successful, otherwise continue
      steps <- steps + 1                #count steps
    }
    return(steps)
  }
  else{
    #strategy3 
    start <- sample(v1, n, replace=FALSE) #n from 2n randomly selected boxes 
    b <- 0
    for (number in start){ 
      if (number == k){ #check if each prisoner found their own number 
        b <- b + 1
        break
      }else{
        b <- b + 1
      }
    }
    steps <- b          #emmmmmm.....
    if (steps == n){
      if (start[n] != k)
        steps <- steps + 1
    }else{
      steps <- steps
    }
    return(steps)
  }
}

pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  y <- rep(0,nreps)
  for(i in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1))
    v2 <- sample(v1, 2*n, replace=FALSE)
    y[i] <- strategies(n,k,strategy,v1,v2) #call strategies function and put steps of each simulation in vector y
  }
  succeed <- length(subset(y,y<=n)) #count times of succeed
  prob = succeed/nreps 
  return(prob) #probability of a single prisoner succeeding in finding their number
}

pone(5,3,1,10000) #Output [1] 0.501

pone(5,3,2,10000) #Output [1] 0.4001

pone(5,3,3,10000) #Output [1] 0.4948

pone(50,5,1,10000)#Output [1] 0.4885

pone(50,5,2,10000)#Output [1] 0.3834

pone(50,5,3,10000)#Output [1] 0.4982





pall <- function(n,strategy,nreps){
    z<- c()
    for (j in 1:nreps){
      y<-c()
      v1 <- c(seq(1, 2*n, by=1)) #v1: number of boxes  
      v2 <- sample(v1, 2*n, replace=FALSE) #v2:random number in boxes
      for (k in 1:(2*n)){
        y[k]<-strategies(n,k,strategy,v1,v2)
      }
      if(length(subset(y,y>n)) >= 1){ #count times of loss
        z[j]<-0
      }else{ 
        z[j]<-1
      }
    }
    x<-length(subset(z,z>=1)) #count times of succeed
    prob<-x/nreps
    return(prob) #probability of all prisoners finding their number
}


pall(5,1,10000)
#Output [1] 0.3546
pall(5,2,10000)
#Output [1] 3e-04
pall(5,3,10000)
#Output [1] 0.0009783905
pall(50,1,10000)
#Output [1] 0.3045
pall(50,2,10000)
#Output [1] 0
pall(50,3,10000)
#Output [1] 7.101905e-31

"calculate the probability with strategy 1 gives output around 30%, the length of the maximum loops formed will be less than n and so every prisoner will be able to find his number.
Which is same as the probability that a uniformly random permutation of the numbers from 1 to 2n.
If each prisoner picks boxes totally at random (strategy3) or picks boxes randomly at the beginning, the probability is almost 0 that they will escape."


dloop <- function(n,nreps){
  loopr <- c()
  for(i in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1))
    v2 <- sample(v1, 2*n, replace=FALSE)
    A <- cbind(v1,v2)
    loopn <- c()
    for (k in 1:(2*n)){
      start <- A[k,1]
      steps <- 1
      p <- start == A[k,2]
      while (p == FALSE) {
        k <- match(c(A[k,2]),v1)
        p <- start == A[k,2]
        steps <- steps + 1
      }
      loopn[k] <- steps
    }
    loopr = append(loopr, loopn)
  }
  sort_loopr <- sort(loopr)
  uni_loopr <- unique(sort_loopr)
  ma_loopr <- match(sort_loopr,uni_loopr)
  prob_loopr <- tabulate(ma_loopr)/length(loopr)
  loop <- rep(0, 2*n)
  loop[uni_loopr] <- prob_loopr
  return(loop)
}

#example code for n=50 
dloop(50,10000)

sum(dloop(50,10000)[1:50]) #probability that there is no loop longer than 50 in a random reshuffling of cards to boxes
# Output [1] 0.496321
