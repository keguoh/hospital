#### http://www.statisticsblog.com/2011/10/waiting-in-line-waiting-on-r/ ####
tmax <- 20
t <- 0
numbServers = 40
# Total time to track
precision = 1000  #precision
epochSeq = seq(1, tmax*precision, 1)
meanServiceTime = 1

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
  return(floor(X*precision))
}
l = 35
b = 10/35
g = 1
lambda <- function(t)  l*(1+b*sin(g*t))
arrivalEpochs <- get_nhpp_realization(lambda)

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
queueLengths = rep(20.5, length(epochSeq))
totalCustomers = 0
serviceCompletionEpoch = rep(0, numbServers)
waitEpoch = c()
leavingTimes = c()
queue = list()
frontOfLineWaits = c()
Akt = rep(0, length(arrivalEpochs))
Dkt = rep(0, length(arrivalEpochs))
Tkt = rep(0, length(arrivalEpochs))

#### Main loop ####
for(i in epochSeq) {
  # Check if anyone is leaving the servers
  for(j in 1:numbServers) {
    if(serviceCompletionEpoch[j] == i) {
      inc(Dkt[totalCustomers])
      dec(totalCustomers)
      # They are leaving the queue, slot to 0
      serviceCompletionEpoch[j] = 0
      leavingTimes = c(leavingTimes, i)
    }
  }
  
  
  # See if a new person is to be added to the queue
  if(i %in% arrivalEpochs) {
    inc(Akt[totalCustomers])
    inc(totalCustomers)
    newPerson = as.proto(person$as.list())
    newPerson$intervalArrived = i
    queue = c(queue, newPerson)
  }
  
  # Can we place someone into a slot?
  for(j in 1:numbServers) {
    # If this slot is free
    if(!serviceCompletionEpoch[j]) { 
      if(length(queue) > 0) {
        placedPerson = queue[[1]]
        serviceCompletionEpoch[j] = i + placedPerson$serviceEpochsNeeded
        waitEpoch = c(waitEpoch, placedPerson$serviceEpochsWaited)
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
  
  # End of the interval, what is the state of things
  queueLengths[i] = length(queue)
  inc(Tkt[totalCustomers])

}

#### Output ####
plot(queueLengths, type="o", col="blue", pch=20, main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
# plot(totalCustomers, type="o", col="blue", pch=20, 
#      main="Total Customers over time", xlab="Person", ylab="Wait time")

plot(Akt/Tkt, xlim = c(0,200))
plot(Dkt/Tkt, xlim = c(0,200))
