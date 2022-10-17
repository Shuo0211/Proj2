#strategy1
strategy1 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)
  a <- A[k,1]
  steps <- 1
  p <- a == A[k,2]
  while (p == FALSE) {
    k <- match(c(A[k,2]),v1)
    p <- a == A[k,2]
    steps <- steps + 1
  }
  return(steps)
}

#strategy 2
strategy2 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)
  a <- sample(v1, 1, replace=FALSE)  #number of box
  steps <- 1
  p <- A[a,2] == k
  while (p == FALSE && steps<(2*n)) {
    a <- match(c(A[a,2]),v1)
    p <- A[a,2] == k
    steps <- steps + 1
  }
  return(steps)
}

#strategy 3
strategy3 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  a <- sample(v1, n, replace=FALSE)
  b <- 0
  for (p in a){
    if (p == k){
      b <- b + 1
      break
    }else{
      b <- b + 1
    }
  }
  steps <- b
  if (steps == n){
    if (a[n] != k)
      steps <- steps + 1
  }else{
    steps <- steps
  }
  return(steps)
}


pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  y <- rep(0,nreps)
  if(strategy == "strategy1"){
    for(i in 1:nreps){
      strategy1(n,k)
      y[i]<-strategy1(n,k)
    }
    x<-length(subset(y,y<=n))
    prob=x/nreps
    return(prob)
  }
  
  else if(strategy == "strategy2"){
    for(i in 1:nreps){
      strategy2(n,k)
      y[i]<-strategy2(n,k)
    }
    x<-length(subset(y,y<=n))
    prob=x/nreps
    return(prob)
  }
  
  else
  {
    for(i in 1:nreps){
      strategy3(n,k)
      y[i]<-strategy3(n,k)
    }
    x<-length(subset(y,y<=n))
    prob=x/nreps
    return(prob)
  }
}

pall <- function(n,strategy,nreps){
  v <- sum(1/c(seq(n+1, 2*n, by=1)))
  if(strategy == "strategy3"){
    l <- c()
    b <- c(1)
    prob <- c(0)
    for (k in 1:(2*n)){
      l[k] <- pone(n,k,strategy,nreps)
      prob <- b * l[k]
      b <- prob
    }
  }else{
    b<-1-v
  }
  return(b)
}


#example codes
pone(5,1,"strategy1",10000)   #Output: prob = 0.4929
pone(5,1,"strategy2",10000)   #Output: prob = 0.4092
pone(5,1,"strategy3",10000)   #Output: prob = 0.4966
pone(50,1,"strategy1",10000)  #Output: prob = 0.4992
pone(50,1,"strategy2",10000)  #Output: prob = 
pone(50,1,"strategy3",10000)  #Output: prob = 0.5053


pall(5,"strategy1",10000)     #Output: prob = 0.3543651
pall(5,"strategy2",10000)     #Output: prob = 0.3543651
pall(5,"strategy3",10000)     #Output: prob = 0.001034689
pall(50,"strategy1",10000)    #Output: prob = 0.3118278
pall(50,"strategy2",10000)    #Output: prob = 0.3118278
pall(50,"strategy3",10000)    #Output: prob = 8.29235e-31



dloop <- function(n,nreps){
  loopr <- c()
  for(i in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1))
    v2 <- sample(v1, 2*n, replace=FALSE)
    A <- cbind(v1,v2)
    loopn <- c()
    for (k in 1:(2*n)){
      a <- A[k,1]
      steps <- 1
      p <- a == A[k,2]
      while (p == FALSE) {
        k <- match(c(A[k,2]),v1)
        p <- a == A[k,2]
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

#example code
dloop(50,10000) 


