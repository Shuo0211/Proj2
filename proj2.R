#strategy1
strategy1 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)
  #i <- sample(v1, 1, replace=FALSE) #number of prisoner #start
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

#strategy 2 (Revised)
strategy2 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)
  #i <- sample(v1, 1, replace=FALSE) #number of prisoner #start
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

#strategy 3 (Revised)
strategy3 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  #i <- sample(v1, 1, replace=FALSE) #number of prisoner #start
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


#The probability that prisoner k draws a card with strategy 1 and succeeds.
circle1<-function(n,k,nreps){
  y <- rep(0,nreps)
  #用来记录nreps次模拟中每次囚犯查找盒子的次数
  for(i in 1:nreps){
    strategy1(n,k)
    #按照选定的strategy运行，并把找到正确数字所打开的盒子数量记录到列表中
    y[i]<-strategy1(n,k)
  }
  x<-length(subset(y,y<=n))
  #将y列表中小于n次的实验摘出，并计算其个数为x
  prob=x/nreps
  #求成功的次数占总实验次数的比例
  return(prob)
}

#The probability that prisoner k draws a card with strategy 2 and succeeds.
circle2<-function(n,k,nreps){
  y <- rep(0,nreps)
  for(i in 1:nreps){
    strategy2(n,k)
    y[i]<-strategy2(n,k)
  }
  x<-length(subset(y,y<=n))
  prob=x/nreps
  return(prob)
}

#The probability that prisoner k draws a card with strategy 3 and succeeds.
circle3<-function(n,k,nreps){
  y <- rep(0,nreps)
  for(i in 1:nreps){
    strategy3(n,k)
    y[i]<-strategy3(n,k)
  }
  x<-length(subset(y,y<=n))
  prob=x/nreps
  return(prob)
}

#1 函数pone
pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  switch(strategy,
         strategy1 = circle1(n,k,nreps),
         strategy2 = circle2(n,k,nreps),
         strategy3 = circle3(n,k,nreps))
}

#2
pall<-function(n,strategy,nreps){
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