#strategy2&3很容易遇到循环 是否应该在strategy里加入判断循环或者抽盒子次数的语句？
#比如 判断抽盒子次数是否大于2n或者不允许重复抽盒子？ 不知道是否允许重复抽盒子（即循环的情况）

#strategy1 改的位置：比较对象是囚犯的编号k 
strategy1 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)  #start
  a <- A[k,1] #第一个盒子里的编码，放在下面循环
  steps <- 1
  p <- k == A[k,2]
  while (p == FALSE) {
    a <- match(c(A[a,2]),v1)
    p <- k == A[a,2]
    steps <- steps + 1
  }
  return(steps)
}

#strategy 2
strategy2 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)
  a <- sample(v1, 1, replace=FALSE) #the first box is random
  steps <- 1
  p <- A[a,2] == k
  while (p == FALSE) {
    a <- match(c(A[a,2]),v1)
    p <- A[a,2] == k
    steps <- steps + 1
  }
  return(steps)
}

#strategy3  既然p已经表示的是随机的数字，那参数i是否还需要？ 容易出现循环或者重复抽箱的可能
strategy3 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  i <- sample(v1, 1, replace=FALSE) #the first box 
  v2 <- sample(v1, n, replace=FALSE)
  steps <- 0
  for (p in a){
    if (p == k){
      steps <- steps + 1
      break
    }
    else{
      steps <- steps + 1
    }
  }
  return(steps)
}

#1 写的有些复杂 再想想策略调用的地方怎么化简
pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  y <- rep(0,nreps)
  #用来记录nreps次模拟中每次囚犯查找盒子的次数
  if(strategy == "strategy1"){
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