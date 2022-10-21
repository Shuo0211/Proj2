"members of group: Yan Chen s2318048, Shuo Wang s2439249, Yuxin Yang s2163400" 
"address of github repo: https://github.com/Shuo0211/Proj2.git"
"Team work contribution: 
@Shuo Wang takes part in strategies function and optimizes the computer cost for 
pall and dloop functions.  
@Yuxin Yang focuses on pall, dloop function and the discription of overview. 
@Yan Chen takes part in pone function，revises strategies function and visualizes
dloop function for n=1:50. 
Proportion of the work for each member is about 1/3. All three members
work together and discuss closely to figure out the project. "  

"Overview:
The whole file is about the prisoner problem -- there are 2*n numbered prisoners
and 2*n boxes, in which are random distributed cards with prisoners' numbers. 
The prisoners one after another opened the boxes to find their numbers. All get
succeed and can be freed only when each of them found his own number with opening
equal to or less than n boxes.
The file gives three strategies that the prisoners may choose, and individual & 
joint probabilities for the prisoners to get freed under each strategy.
In addition, it also calculates the probabilities of the depth of nesting for
strategy 1 and 2."

"Three strategies:
1: the prisoner starts at the box with their number, 
opens it and reads the number on card, then goes to the box with the same 
number with the former card if not the prisoner number.
2: the prisoner starts at a random box that the prisoner chooses, 
opens it and reads the number on card, then goes to the box with the same 
number with the former card if not the prisoner number.
3: the prisoner chooses and opens half amount of box at one time and
see if the prisoner's number is on the card in the boxes."

"Overall common function parameters:
n - half number of prisoners
k - prisoner number 
strategy - tell which strategy to choose
nreps - the number of replicate simulations."


"
'strageties' function: returns the amount of the boxes the prisoner k opened till
he found his number.
"

strategies <- function(n, k, strategy, cards){ 
  if (strategy == 1){   # strategy 1
    current <- k        # k-th prisoner starts with the k-th box
    steps <- 1
    # check whether the prisoner found his number each time he opened a new box
    found <- k == cards[current]
    while (found == FALSE) {
      current <- cards[current]    # goes to the next box
      found <- k == cards[current]
      steps <- steps + 1           # count steps
    }
    return(steps)
  }
  else if (strategy == 2){         # strategy 2 
    current  <- sample(1:(2*n), 1) # starts with a random box
    steps <- 1
    # check whether the prisoner found his number each time he opened a new box
    found <- k == cards[current] 
    while (found == FALSE && steps < (2*n)) { 
      current <- cards[current]    # goes to the next box
      found <- k == cards[current]          
      steps <- steps + 1           # count steps
    }
    return(steps)
  }
  else{       # strategy 3
	# randomly choose n boxes to open
    chosen_boxes <- sample(1:(2*n), n, replace=FALSE) 
    steps <- 1
    # check if the number found each time opening a box
    for (number in chosen_boxes){ 
      if (number == k)
        break
	  steps <- steps + 1
    }
    return(steps)
  }
}

"
'pone' function: returns the probability of a single prisoner succeeding 
in finding his number.
"

pone<-function(n, k, strategy, nreps){ 
  success <- 0           # count of successful simulations
  # check in each simulation whether the prison succeed or not
  for(i in 1:nreps){
    cards <- sample(1:(2*n), 2*n, replace=FALSE) # random distributed cards in boxes
    steps <- strategies(n,k,strategy,cards)      # get the steps the prisoner need
	if(steps <= n)
		success <- success + 1
  }
  prob = success/nreps 
  return(prob) 
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
prisoner used strategy 3 to find his number is around 50%. Strategy 1 gives 
almost the same probability as strategy 3. Compare to strategy 1 and 3, strategy
2 gives less probability around 40%. Overall, more random choose, fewer 
probability to be succeed "


"
'pall' function: returns the probability of all prisoners succeeding in
finding their numbers.
"

pall <- function(n,strategy,nreps){
  fail <- 0              # count of unsuccessful simulations
  # check in each simulation whether all of the prisoners succeed or not
  for (j in 1:nreps){
    cards <- sample(1:(2*n), 2*n, replace=FALSE)#random distributed cards in boxes
    steps <- 0
	# check each prisoner
    for (k in 1:(2*n)){
      steps <- strategies(n,k,strategy,cards) #get the steps the prisoner need
      if (steps > n){
        fail <- fail + 1 # k failed means all of them failed as a whole
        break
      }
    }
  }
  prob <- 1 - fail / nreps
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
#Output [1] 0

"Comments: calculation of the probability with strategy 1 gives output around 30%, 
the length of the maximum loops formed will be less than n and so every prisoner 
will be able to find his number, which is same as the probability that a 
uniformly random permutation of the numbers from 1 to 2n. If each prisoner 
picks boxes totally at random(strategy3) or picks boxes randomly at the 
beginning(strategy2), the probability is almost 0 that they will escape. 
The larger n, the smaller the probability that all prisoner will escape. 
More random choose, fewer probability to be succeed"


"
'dloop' function: returns the probability of each loop length from 1 to 
prisoner number occurring at least once in a random shuffling of cards in 
boxes.
"

dloop <- function(n, nreps){
  # occur_vec[j]: how many times the loop with length j occurs in nreps simulations
  # j = 1...2n
  occur_vec <- rep(0, 2*n)
  # check the occurence of the loops in each simulation
  for(i in 1:nreps){
    cards <- sample(1:(2*n), 2*n, replace=FALSE) # random distributed cards in boxes
    # mark_vec[j]: mark whether the j-length-loop exists already or not
    # mark_vec[j] == 0 for 'not exist' and mark_vec[j] == 1 for 'exist'
    mark_vec <- rep(0, 2*n) 
    # avoid_vec: stores prisoner numbers that already appear in a loop
    avoid_vec <- c()  
    for(k in 1:(2*n)){
      if(k %in% avoid_vec) 
        next    # reason: all prisoners in the same loop  have the same length
      current <- k
      steps <- 1
      found <- k == cards[current]
      while(found == FALSE){
        # stores all numbers of cards from a single loop in avoid_vec
        avoid_vec = append(avoid_vec, cards[current]) 
        current <- cards[current]
        found <- k == cards[current]
        steps <- steps + 1
      }
      # found the steps-length-loop, so mark it in the mark_vec
      if(mark_vec[steps] == 0)
        mark_vec[steps] <- 1 
    }
    # update the number of times that the loops with length 1:2n appear as the 
    # simulations run
    occur_vec <- occur_vec + mark_vec 
  }
  prob_vec <- occur_vec / nreps
  return(prob_vec)
}


# example code for n = 50 
dloop(50, 10000)

# probability that there is no loop longer than 50 in a random reshuffling of 
# cards to boxes
dl <- dloop(50, 10000)[1:50]
sum(dl)
# Output [1] 3.855


plot(dl,main="the probability no loop longer than 50",xlab="the length of a loop",ylab="the probability of each loop")
lm(formula=dl~c(1:50))  # linear regressions on the data to assess overall trends
abline(lm(dl~c(1:50)))
