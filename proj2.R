strategy <- function(i,n,k,v1,v2){
  A <- cbind(v1,v2)
  if (i==1){
    #strategy[1] <- function(n,k){
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
  else if (i==2){
    #strategy[2] <- function(n,k){
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
  else{
    #strategy[3] <- function(n,k){
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
}

pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  y <- c()
  for(i in 1:nreps){
    v1 <- c(seq(1, (2*n), by=1))
    v2 <- sample(v1, (2*n), replace=FALSE)
    strategy(strategy,n,k,v1,v2)
    y[i]<-strategy(strategy,n,k,v1,v2)
  }
  x<-length(subset(y,y<=n))
  prob=x/nreps
  return(prob)
}

pall <- function(n,strategy,nreps){
  z<- c()
  for (j in 1:nreps){
    v1 <- c(seq(1, (2*n), by=1))
    v2 <- sample(v1, (2*n), replace=FALSE)
    y<-c()
    for (l in 1:(2*n)){
      strategy(strategy,n,l,v1,v2)
      y[l]<-strategy(strategy,n,l,v1,v2)
    }
    if(length(subset(y,y>n)) >1){
      z[j]<-0
    }else{
      z[j]<-1
    }
  }
  x<-length(subset(z,z==1))
  prob<-x/nreps
  return(prob)
}

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


#不知道统计量是否可以直接用概率来算  明天再问问其他同学
pall <- function(n,strategy,nreps){
  if(strategy == 3){
    l <- c()
    prob <- c(1)
    b <- c(0)
    for (k in 1:(2*n)){
      l[k] <- pone(n,k,strategy,nreps)
      b <- prob * l[k]
      prob <- b
    }else{
      
      for (k in 1:(2*n)){
        
        
      }
    }
    return(prob)
  }
  
