#member of groups: Yan Chen s2318048, Shuo Wang s2439249, Yuxin Yangs2163400 
#address of github repo: https://github.com/Shuo0211/Proj2.git
#contribution:

"Overview: The whole file is about the prisoner problem which is that 
the prisoners choose half amount of the boxes and find the card in boxes 
writing the same number with themselves. The number of the boxes is the 
same with the number of prisoners and all can be free only with all the 
prisoners finding the same number on card. The file includes three 
strategies and individual & joint probabilities for the prisoners to be 
free in each strategy. In addition, it also includes the probabilities of 
the depth of nesting for strategy 1 and 2."

"Overall common arguments: n - half number of prisoners; k - prisoner number; 
strategy - tell which strategies to choose; nreps - the number of replicate 
simulations."

"'strageties' function: it includes three strategies. 
- First one is the prisoner starts at the box with their number, 
opens it and reads the number on card, then goes to the box with the same 
number with the former card if not the prisoner number.
- Second one is the prisoner starts at a random box that the prisoner chooses, 
opens it and reads the number on card, then goes to the box with the same 
number with the former card if not the prisoner number.
- Third one is the prisoner chooses and opens half amount of box at one time and
see if the prisoner's number is on the card in the boxes. "

strategies <- function(n,k,strategy,v1,v2){ 
  A <- cbind(v1,v2)    #v1: number of boxes  v2:random number in boxes
  if (strategy == 1){  #strategy1
    start <- A[k,1]    #k-th prisoner opens the k-th box
    steps <- 1
    found <- start == A[k,2] 
    #check if k-th prisoner found number k from the k-th box
    while (found == FALSE && steps<(2*n)) {
      k <- match(c(A[k,2]),v1) 
      #find the next box labeled with the number found before
      found <- start == A[k,2] 
      #If this drawer contains their number, they are done and were successful
      steps <- steps + 1  #count steps
    }
    return(steps)
  }
  else if (strategy==2){ #strategy2 
    start  <- sample(v1, 1, replace=FALSE)  #select a box randomly
    steps <- 1
    found <- A[start,2] == k 
    #check if k-th prisoner found number k from the k-th box
    while (found == FALSE && steps<(2*n)) { 
      start <- match(c(A[start,2]),v1)  
      #find the next box labeled with the number found before
      found <- A[start,2] == k          
      #If this drawer contains their number, they are done and were successful
      steps <- steps + 1#count steps
    }
    return(steps)
  }
  else{
    #strategy3 
    start <- sample(v1, n, replace=FALSE) 
    #the card number in n random choosing boxes
    b <- 0
    for (number in start){  #check if the number on card is the prisoner number
      if (number == k){  #count the steps in b, if is the prisoner number, break
        b <- b + 1 
        break
      }else{
        b <- b + 1
      }
    }
    steps <- b    #put the count b in steps      
    if (steps == n){  
    #if the steps is the length of choosing, check if it is the prisoner number
      if (start[n] != k)
        steps <- steps + 1
      #if it is not prisoner number, add one to step which is out of range
    }else{
      steps <- steps
    }
    return(steps)
  }
}

"'pone' function: it includes the probability of a single prisoner succeeding 
in finding their number under the number of replicate simulations."

pone<-function(n,k,strategy,nreps){ 
  #n:amount of boxes, k: prisoner number, nreps: times of simulation
  y <- rep(0,nreps)
  for(i in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1)) 
    #v1: sequence of the boxes
    v2 <- sample(v1, 2*n, replace=FALSE) 
    #v2: sequence of random distributed numbers in the boxes 
    y[i] <- strategies(n,k,strategy,v1,v2) 
    #call strategies function and put steps of each simulation in vector y
  }
  succeed <- length(subset(y,y<=n)) #count times of succeed
  prob = succeed/nreps 
  return(prob) 
  #probability of a single prisoner succeeding in finding their number
}

pone(5,3,1,10000) 
#Output [1] 0.501
pone(5,3,2,10000) 
#Output [1] 0.4001
pone(5,3,3,10000) 
#Output [1] 0.5069
pone(50,5,1,10000) 
#Output [1] 0.4885
pone(50,5,2,10000) 
#Output [1] 0.3834
pone(50,5,3,10000) 
#Output [1] 0.5063

"Comments: strategy 3 shows the best solution, the probability that a single 
prisoner used strategy 3 to find their number is around 50%. Strategy 1 gives 
almost thesame probability as strategy 3. Compare to strategy1 and 3 strategy2 
gives less probability around 40%. Overall, more random choose, fewer 
probability to be succeed "


"'pall' function: it includes the probability of all prisoners succeeding in
finding their number under the number of replicate simulations and going free."

pall <- function(n,strategy,nreps){
  fail <- 0 # count of unsuccessful tries
  for (j in 1:nreps){
    v1 <- c(seq(1, 2*n, by=1)) 
    #v1: sequence of the boxes
    v2 <- sample(v1, 2*n, replace=FALSE) 
    #v2: sequence of random distributed numbers in the boxes 
    steps <- 0
    for (k in 1:(2*n)){
      steps <- strategies(n,k,strategy,v1,v2) 
      #y: count of steps that the k-th prisoner used 
      if (steps > n){
        fail <- fail+1 #k failed means all of them failed as whole
        break
      }
    }
  }
  prob<- 1-fail/nreps
  return(prob) #probability of all prisoners finding their number
}


pall(5,1,10000)
#Output [1] 0.3546
pall(5,2,10000)
#Output [1] 1e-04
pall(5,3,10000)
#Output [1] 0.0009783905     
pall(50,1,10000)
#Output [1] 0.3045
pall(50,2,10000)
#Output [1] 0
pall(50,3,10000)
#Output [1] 0

"Comments: calculate the probability with strategy 1 gives output around 30%, 
the length of the maximum loops formed will be less than n and so every prisoner 
will be able to find his number, which is same as the probability that a 
uniformly random permutation of the numbers from 1 to 2n. If each prisoner 
picks boxes totally at random(strategy3) or picks boxes randomly at the 
beginning(strategy2), the probability is almost 0 that they will escape. 
The larger n, the smaller the probability that all prisoner will escape. 
More random choose, fewer probability to be succeed"

"'dloop' function: it includes the probability of each loop length from 1 to 
prisoner number occurring at least once in a random shuffling of cards to 
boxes."

dloop <- function(n, nreps){
  occur_vec <- rep(0, 2*n)
  #occur_vec[j]: how many times the circle with length j occurs in the nreps experiments
  #j=1...2n
  for(i in 1:nreps){
    cards <- sample(1:(2*n), 2*n, replace=FALSE) 
    #cards: cards in the boxes, cards[j] represents the card number in j-th box
    mark_vec <- rep(0, 2*n) 
    #mark_vec[j]: mark whether the j-length-circle exists already or not
    avoid_vec <- c()  
    #avoid_vec: stores elements that already occur in a circle
    for(k in 1:(2*n)){
      if(k %in% avoid_vec) 
        next #reason: all elements in a circle have the same length
      start <- k
      steps <- 1
      check <- start == cards[k]
      while(check == FALSE){
        avoid_vec = append(avoid_vec, cards[k]) 
        #stores all numbers of cards from a single circle in avoid_vec
        k <- cards[k]
        check <- start == cards[k]
        steps <- steps + 1
      }
      if(mark_vec[steps] == 0)
        mark_vec[steps] <- 1 
        #count times each circle with length 1:2n occurs
    }
    occur_vec <- occur_vec + mark_vec 
    #update times each circle with length 1:2n occurs under nreps simulations
  }
  prob_vec <- occur_vec / nreps
  return(prob_vec)
}


dloop(50,10000)
#example code for n=50 

dl<-dloop(50,10000)[1:50]
sum(dl)
#probability that there is no loop longer than 50 in a random reshuffling of 
#cards to boxes
# Output [1] 0.496321


plot(dl,main="the probability no loop longer than 50",xlab="the length of a loop",ylab="the probability of each loop")
lm(formula=dl~c(1:50))# linear regressions on the data to assess overall trends
abline(lm(dl~c(1:50)))