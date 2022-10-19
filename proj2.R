strategies <- function(n,k,strategy,v1,v2){
  A <- cbind(v1,v2)
  if (strategy == 1){
    #strategy[1]
    a <- A[k,1]
    steps <- 1
    p <- a == A[k,2]
    while (p == FALSE && steps<(2*n)) {
      k <- match(c(A[k,2]),v1)
      p <- a == A[k,2]
      steps <- steps + 1
    }
    return(steps)
  }
  else if (strategy==2){
    #strategy[2] 
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
    #strategy[3] 
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
  y <- rep(0,nreps)
  for(i in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1))
    v2 <- sample(v1, 2*n, replace=FALSE)
    y[i]<-strategies(n,k,strategy,v1,v2)
  }
  x<-length(subset(y,y<=n))
  prob=x/nreps
  return(prob)
}

pone(5,3,1,10000)
#Output [1] 0.501
pone(5,3,2,10000)
#Output [1] 0.4001
pone(5,3,3,10000)
#Output [1] 0.4948
pone(50,5,1,10000)
#Output [1] 0.4885
pone(50,5,2,10000)
#Output [1] 0.3834
pone(50,5,3,10000)
#Output [1] 0.4982

pall <- function(n,strategy,nreps){
  if(strategy == 3){
    l <- c()
    prob <- c(1)
    b <- c(0)
    for (k in 1:(2*n)){
      l[k] <- pone(n,k,strategy,nreps)
      b <- prob * l[k]
      prob <- b
    }
  }else{
    z<- rep(0,nreps)
    for (j in 1:nreps){
      y<-c()
      v1 <- c(seq(1, 2*n, by=1))
      v2 <- sample(v1, 2*n, replace=FALSE)
      for (k in 1:(2*n)){
        y[k]<-strategies(n,k,strategy,v1,v2)
      }
      if(length(subset(y,y>n)) >= 1){
        z[j]<-0
      }else{
        z[j]<-1
      }
    }
    x<-length(subset(z,z>=1))
    prob<-x/nreps
  }
  return(prob)
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

dl<-dloop(50,10000)[1:50]
sum(dl)
# Output [1] 0.496321
plot(dl,main="the probability no loop longer than 50",xlab="the length of a loop",ylab="the probability of each loop")
lm(formula=dl~c(1:50))# linear regressions on the data to assess overall trends
abline(lm(dl~c(1:50)))
