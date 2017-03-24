library(proto)
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


set.seed(1)
arrivalEpochs <- sort(sample(arrivals, round(length(arrivals)/44), replace = F))
timeline <- 1:(3600*17)
serviceTimeSeq <- sample(serviceTime, length(arrivalEpochs), replace = T)
patientTimeSeq <- sample(patientTime, length(arrivalEpochs), replace = T)

numbServers = 3

totalCustomers = 0
totalAbandons = 0
serviceCompletionEpoch = rep(0, numbServers)
k = 1 # counting arrival orders
abandonEpoch = c()
queue = list()
frontOfLineWaits = c()
queueLengths = rep(20.5, length(timeline))
numbCustomers = rep(20.5, length(timeline))
waitEpoch = c()

ptm1 = proc.time()

#### Main loop ####
for(i in timeline) {
  # Check if anyone is leaving the servers
  for(j in 1:numbServers) {
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
  for(j in 1:numbServers) {
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
ptm2 = proc.time()
t_sim = ptm2[3] - ptm1[3]
cat("The simulation takes", t_sim, "s" )

L = list()
for(i in 1:17){
  t = table(numbCustomers[(3600*(i-1)+1):(3600*i)])
  L[[i]] = matrix(
    c(sort(as.numeric(paste(dimnames(t)[[1]]))),
      cumsum(sort(as.vector(t)/3600))), ncol = 2) # reverse 
}
L[[1]]

alpha = .2

plot(queueLengths[1:3600*5], type="o", col="blue", pch=20, main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
plot(numbCustomers[1:3600*5], type="o", col="blue", pch=20, main="# of Customers over time",
     xlab="Interval", ylab="# of customers in system")
totalAbandons
totalAbandons/length(arrivalEpochs)
