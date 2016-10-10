## NHPP Arrivals
get_nhpp_realization <- function(lambda){
  set.seed(1)
  t_max <- 10
  t <- 0
  lambda_star <- function(){
    max(sapply(seq(1, t_max,length.out=1000), lambda))*2}
  Lambda <- function(tupper){
    integrate(f = lambda, lower = 0, upper = tupper)$value}
  X <- numeric()
  while(t <= t_max){
    u <- runif(1)
    t <- t - log(u)/lambda_star()
    if(runif(1) < lambda(t)/lambda_star()) {
      X <- c(X,t)
    }
  }
  return(X)
}
l = 35
b = 10/35
g = 1
lambda <- function(t)  l*(1+b*sin(g*t))
res_1 <- get_nhpp_realization(lambda)

#### Code by Matt Asher. Published at StatisticsBlog.com ####
#### http://www.statisticsblog.com/2011/10/waiting-in-line-waiting-on-r/ ####
#### CONFIG ####
# Number of slots to fill
numbSlots = 40

prec = 1000  #precision
t = 0
tmax = 10
At = floor(res_1*prec)/prec
# Total time to track
intervals = seq(t,tmax,1/prec)

# Average time each person takes at the teller, discretized exponential 
# distribution assumed Times will be augmented by one, so that everyone takes at
# least 1 interval to serve
meanServiceTime = 1

#### INITIALIZATION ####
queueLengths = rep(0, length(intervals))
totalCustomer = 0
slots = rep(0, numbSlots)
waitTimes = c()
leavingTimes = c()
queue = list()
arrivalTimes = c()
frontOfLineWaits = c()
Akt = rep(0, length(At))
Dkt = rep(0, length(At))
Tkt = rep(0, length(At))

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
  intervalAttended = NULL,
  # How much teller time will this person demand?
  intervalsNeeded = floor(rexp(1, 1/meanServiceTime)) + 1,
  intervalsWaited = 0,
  intervalsWaitedAtHeadOfQueue = 0
)

#### Main loop ####
for(i in intervals) {
  
  # Check if anyone is leaving the slots
  for(j in 1:numbSlots) {
    if(slots[j] == i) {
      inc(Dkt[totalCustomer])
      dec(totalCustomer)
      # They are leaving the queue, slot to 0
      slots[j] = 0
      leavingTimes = c(leavingTimes, i)
    }
  }
  
  
  # See if a new person is to be added to the queue
  if(i %in% At) {
    inc(Akt[totalCustomer])
    inc(totalCustomer)
    newPerson = as.proto(person$as.list())
    newPerson$intervalArrived = i
    queue = c(queue, newPerson)
    arrivalTimes  = c(arrivalTimes, i)
  }
  
  # Can we place someone into a slot?
  for(j in 1:numbSlots) {
    # If this slot is free
    if(!slots[j]) { 
      if(length(queue) > 0) {
        placedPerson = queue[[1]]
        slots[j] = i + placedPerson$intervalsNeeded
        waitTimes = c(waitTimes, placedPerson$intervalsWaited)
        # Only interested in these if person waited 1 or more intevals at front
        # of line
        if(placedPerson$intervalsWaitedAtHeadOfQueue) {
          frontOfLineWaits = c(frontOfLineWaits, 
                               placedPerson$intervalsWaitedAtHeadOfQueue)
        }
        
        # Remove placed person from queue
        queue[[1]] = NULL
      }
    }
  }
  
  # Everyone left in the queue has now waited one more interval to be served
  if(length(queue)) {
    for(j in 1:length(queue)) {
      inc(queue[[j]]$intervalsWaited) # = queue[[j]]$intervalsWaited + 1
    }
    
    # The (possibley new) person at the front of the queue has had to wait 
    # there one more interval.
    inc(queue[[1]]$intervalsWaitedAtHeadOfQueue) 
    # = queue[[1]]$intervalsWaitedAtHeadOfQueue + 1
  }
  
  # End of the interval, what is the state of things
  queueLengths[i*prec] = length(queue);
}

#### Output ####
plot(queueLengths, type="o", col="blue", pch=20, 
     main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
# plot(waitTimes, type="o", col="blue", pch=20, main="Wait times", 
#      xlab="Person", ylab="Wait time")