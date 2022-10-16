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
  while (p == FALSE) {
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
  l <- c()
  b <- c(1)
  prob <- c(0)
  for (k in 1:(2*n)){
    l[k] <- pone(n,k,strategy,nreps)
    prob <- b * l[k]
    b <- prob
  }
  return(b)
}

dloop <- function(n,nreps){
  
  
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
    
  }
}


