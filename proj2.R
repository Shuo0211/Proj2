#strategy2&3����������ѭ�� �Ƿ�Ӧ����strategy������ж�ѭ�����߳���Ӵ�������䣿
#���� �жϳ���Ӵ����Ƿ����2n���߲������ظ�����ӣ� ��֪���Ƿ������ظ�����ӣ���ѭ���������

#strategy1 �ĵ�λ�ã��Ƚ϶����������ı��k 
strategy1 <- function(n,k){
  v1 <- c(seq(1, 2*n, by=1))
  v2 <- sample(v1, 2*n, replace=FALSE)
  A <- cbind(v1,v2)  #start
  a <- A[k,1] #��һ��������ı��룬��������ѭ��
  steps <- 1
  p <- k == A[k,2]
  while (p == FALSE) {
    a <- match(c(A[a,2]),v1)
    p <- k == A[a,2]
    steps <- steps + 1
  }
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
}

#strategy3  ���׳���ѭ�������ظ�����Ŀ���
strategy3 <- function(n){
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
}

#1
pone<-function(n,k,strategy,nreps){
  #give the amount of boxes, the number of prisoner, which strategy we chose and the number of 
  y <- rep(0,length(nreps))
  if(strategy == strategy1){
    y <- rep(0,length(nreps))
    #������¼nreps��ģ����ÿ���������Һ��ӵĴ���
    for(i in 1:nreps){
      strategy1(n,k)
      #����ѡ����strategy���У������ҵ���ȷ�������򿪵ĺ���������¼���б���
      y[i]<-strategy1(n,k)
    }
    x<-lengths(subset(y,y<=n))
    #��y�б���С��n�ε�ʵ��ժ���������������Ϊx
    prob=x/nreps
    #��ɹ��Ĵ���ռ��ʵ������ı���
    print(prob)
  }
  
  else if(strategy == strategy2){
    y <- rep(0,length(nreps))
    for(i in 1:nreps){
      strategy2(n)
      y[i]<-strategy2(n)
    }
    x<-lengths(subset(y,y<=n))
    prob=x/nreps
    print(prob)
  }
  
  else(strategy == strategy3)
  {
    y <- rep(0,length(nreps))
    for(i in 1:nreps){
      strategy3(n)
      y[i]<-strategy3(n)
    }
    x<-lengths(subset(y,y<=n))
    prob=x/nreps
    print(prob)
  }
}