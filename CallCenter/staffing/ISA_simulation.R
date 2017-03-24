library(proto)
library(plyr)
arrivals <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/staffing/arrival_pool.csv")$V1 - 7*3600
serviceTime <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/staffing/serv_time_pool.csv")$V1
patientTime <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/staffing/patient_time_pool.csv")$V1

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
  epochArrived = 0,
  # How much teller time will this person demand?
  serviceTimeNeeded = 0,
  serviceTimeWaited = 0,
  serviceTimeWaitedAtHeadOfQueue = 0,
  patientTime = 0
)

numbArrivals <- round(length(arrivals)/44)
N = 3600*17
timeline <- 1:N

ntrials=5

set.seed(1)

simu <- function(numbServers = rep(20, N)){
  Lmatrix <- matrix(0, ntrials, N)
  for (trial in 1:ntrials){
    arrivalEpochs <- sort(sample(arrivals, numbArrivals, replace = F))
    serviceTimeSeq <- sample(serviceTime, length(arrivalEpochs), replace = T)
    patientTimeSeq <- sample(patientTime, length(arrivalEpochs), replace = T)
    
    totalCustomers = 0
    totalAbandons = 0
    serviceCompletionEpoch = rep(0, max(numbServers))
    k = 1 # counting arrival orders
    abandonEpoch = c()
    queue = list()
    frontOfLineWaits = c()
    queueLengths = rep(.5, length(timeline))
    numbCustomers = rep(.5, length(timeline))
    waitEpoch = c()
    
    #### Main loop ####
    for(i in timeline) {
      # Check if anyone is leaving the servers
      for(j in 1:numbServers[i]) {
        if(serviceCompletionEpoch[j] == i) {
          dec(totalCustomers)
          # They are leaving the queue, slot to 0
          serviceCompletionEpoch[j] = 0
        }
      }
      
      # See if a new person is to be added to the queue
      while(i == arrivalEpochs[k] & k <= length(arrivalEpochs)) {
        inc(totalCustomers)
        newPerson = person$proto(epochArrived = i, 
                                 serviceTimeNeeded = serviceTimeSeq[k],
                                 patientTime = patientTimeSeq[k])
        queue = c(queue, newPerson)
        inc(k)
      }
      
      
      # If anyone is dropping the waiting line?
      if(length(queue)) {
        for(j in length(queue):1) {
          if(queue[[j]]$serviceTimeWaited >= queue[[j]]$patientTime){
            queue[[j]] = NULL
            inc(totalAbandons)
            abandonEpoch = c(abandonEpoch, i)
            dec(totalCustomers)
          }
        }
      }
      
      
      # Can we place someone into a slot?
      for(j in 1:numbServers[i]) {
        # If this slot is free
        if(serviceCompletionEpoch[j]==0) { 
          if(length(queue) > 0) {
            placedPerson = queue[[1]]
            serviceCompletionEpoch[j] = i + placedPerson$serviceTimeNeeded
            waitEpoch = c(waitEpoch, placedPerson$serviceTimeWaited)
            # Only interested in these if person waited 1 or more intevals at front
            # of line
            if(placedPerson$serviceTimeWaitedAtHeadOfQueue) {
              frontOfLineWaits = c(frontOfLineWaits, 
                                   placedPerson$serviceTimeWaitedAtHeadOfQueue)
            }
            queue[[1]] = NULL
          }
        }
      }
      
      # Everyone left in the queue has now waited one more interval to be served
      if(length(queue)) {
        for(j in length(queue):1) {
          inc(queue[[j]]$serviceTimeWaited)
        }
        # The (possibley new) person at the front of the queue has had to wait 
        # there one more interval.
        inc(queue[[1]]$serviceTimeWaitedAtHeadOfQueue)
      }
      
      
      # End of the interval, what is the state of things
      queueLengths[i] = length(queue)
      numbCustomers[i] = totalCustomers
    }
    Lmatrix[trial, ] <- numbCustomers
  }
  return(Lmatrix)
}
ptm1 = proc.time()
Lmatrix <- simu()
ptm2 = proc.time()
t_sim = ptm2[3] - ptm1[3]
cat("The simulation takes", t_sim, "s" )


alpha = .2
argmin = function(L){
  L = sort(L)
  L[ntrials - ceiling(alpha*ntrials) + 1] + 1  #Compute based on (3.24)
}

S = apply(Lmatrix, 2, argmin)


ISA <- function(){
  eps = 500
  S0 = rep(20, N)
  while(eps > 2){
    S1 = S0
    Lmatrix <- simu(numbServers = S0)
    S1 = apply(Lmatrix, 2, argmin)
    eps = max(S1-S0)
    cat('one step ..')
  }
  return(S1)
}

Final_S <- ISA()


plot(queueLengths[1:3600*5], type="o", col="blue", pch=20, main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
plot(numbCustomers[1:3600*5], type="o", col="blue", pch=20, main="# of Customers over time",
     xlab="Interval", ylab="# of customers in system")
totalAbandons
totalAbandons/length(arrivalEpochs)
