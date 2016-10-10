#### http://www.statisticsblog.com/2011/10/waiting-in-line-waiting-on-r/ ####
tmax <- 15
t <- 0
numbServers = 10
# Total time to track
precision = 100  #precision
timeSeq = seq(1/precision,tmax,1/precision)
meanServiceTime = .1

#### NHPP Arrivals ####
get_nhpp_realization <- function(lambda){
  set.seed(1)
  lambda_star <- function(){
    max(sapply(seq(1, tmax,length.out=1000), lambda))*2}
  Lambda <- function(tupper){
    integrate(f = lambda, lower = 0, upper = tupper)$value}
  X <- numeric()
  while(t <= tmax){
    u <- runif(1)
    t <- t - log(u)/lambda_star()
    if(runif(1) < lambda(t)/lambda_star()) {
      X <- c(X,t)
    }
  }
  return(floor(X*precision)/precision)
}
l = 35
b = 10/35
g = 1
lambda <- function(t)  l*(1+b*sin(g*t))
arrivalTimes <- get_nhpp_realization(lambda)

# Average time each person takes at the teller, discretized exponential 
# distribution assumed Times will be augmented by one, so that everyone takes at
# least 1 interval to serve


#### Libraries ####
# Use the proto library to treat people like objects in traditional oop
library("proto")

#### Functions ####
# R is missing a nice way to do ++, so we use this
inc <- function(x) {
  eval.parent(substitute(x <- x + 1))
}
dec <- function(x) {
  eval.parent(substitute(x <- x - 1))
}

# Main object, really a "proto" function
person <- proto(
  intervalArrived = 0,
  # How much teller time will this person demand?
  serviceEpochsNeeded = (floor(rexp(1, 1/meanServiceTime)*precision) + 1),
  serviceEpochsWaited = 0,
  serviceEpochsWaitedAtHeadOfQueue = 0
)

#### INITIALIZATION ####
queueLengths = rep(20, length(timeSeq))
totalCustomers = 0
serviceCompletionTime = rep(0, numbServers)
waitTimes = c()
leavingTimes = c()
queue = list()
frontOfLineWaits = c()
Akt = rep(0, length(arrivalTimes))
Dkt = rep(0, length(arrivalTimes))
Tkt = rep(0, length(arrivalTimes))

#### Main loop ####
for(i in timeSeq) {
  # Check if anyone is leaving the servers
  for(j in 1:numbServers) {
    if(serviceCompletionTime[j] == i) {
      inc(Dkt[totalCustomers])
      dec(totalCustomers)
      # They are leaving the queue, slot to 0
      serviceCompletionTime[j] = 0
      leavingTimes = c(leavingTimes, i)
    }
  }
  
  
  # See if a new person is to be added to the queue
  if(i %in% arrivalTimes) {
    inc(Akt[totalCustomers])
    inc(totalCustomers)
    newPerson = as.proto(person$as.list())
    newPerson$intervalArrived = i
    queue = c(queue, newPerson)
  }
  
  # Can we place someone into a slot?
  for(j in 1:numbServers) {
    # If this slot is free
    if(!serviceCompletionTime[j]) { 
      if(length(queue) > 0) {
        placedPerson = queue[[1]]
        serviceCompletionTime[j] = i + placedPerson$serviceEpochsNeeded/precision
        waitTimes = c(waitTimes, placedPerson$serviceEpochsWaited/precision)
        # Only interested in these if person waited 1 or more intevals at front
        # of line
        if(placedPerson$serviceEpochsWaitedAtHeadOfQueue) {
          frontOfLineWaits = c(frontOfLineWaits, 
                               placedPerson$serviceEpochsWaitedAtHeadOfQueue)
        }
        
        # Remove placed person from queue
        queue[[1]] = NULL
      }
    }
  }
  
  # Everyone left in the queue has now waited one more interval to be served
  if(length(queue)) {
    for(j in 1:length(queue)) {
      inc(queue[[j]]$serviceEpochsWaited)
    }
    # The (possibley new) person at the front of the queue has had to wait 
    # there one more interval.
    inc(queue[[1]]$serviceEpochsWaitedAtHeadOfQueue)
  }
  
  if (i<2 & i>1.9){
    print(c(length(queue), i, totalCustomers))
  }
  
  # End of the interval, what is the state of things
  queueLengths[i*precision] = length(queue)
  inc(Tkt[totalCustomers])
  if (i<2 & i>1.9){
    print(c(queueLengths[i*precision], length(queue), i*precision, totalCustomers))
  }
}

#### Output ####
plot(queueLengths, type="o", col="blue", pch=20, main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
# plot(totalCustomers, type="o", col="blue", pch=20, 
#      main="Total Customers over time", xlab="Person", ylab="Wait time")