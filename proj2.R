
#strategy1
n<-10
v1<-c(seq(1, 2*n, by=1))
v2<-sample(v1, 2*n, replace=FALSE)
A<-cbind(v1,v2)
i <- sample(v1, 1, replace=FALSE) #number of prisoner #start
a<-A[i,1]
steps<-1
p<- a == A[i,2]
while (p == FALSE) {
  i<-match(c(A[i,2]),v1)
  p<- a == A[i,2]
  steps<-steps+1
}

