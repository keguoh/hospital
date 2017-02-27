arrivalEpochs <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/arrivalEpochs_tmax10000_prec1000_g1.txt",)$V1
arrivalEpochs <- arrivalEpochs[1:5]
timeline <- seq(1, max(arrivalEpochs), 1)
serviceTimeSeq <- rep(4,5)
patientTimeSeq <- rep(10,5)
#### Functions ####
# R is missing a nice way to do ++, so we use this
inc <- function(x) {
  eval.parent(substitute(x <- x + 1))
}
dec <- function(x) {
  eval.parent(substitute(x <- x - 1))
}

set.seed(1)
# Main object, really a "proto" function
person <- proto(
  epochArrived = 0,
  # How much teller time will this person demand?
  serviceTimeNeeded = 0,
  serviceTimeWaited = 0,
  serviceTimeWaitedAtHeadOfQueue = 0,
  patientTime = 0
)

numbServers = 2
meanServiceTime = 1
meanPatientTime = 1


totalCustomers = 0
totalAbandons = 0
serviceCompletionEpoch = rep(0, numbServers)
k = 1 # counting arrival orders

queueLengths = rep(20.5, length(timeline))
numbCustomers = rep(20.5, length(timeline))
waitEpoch = c()
abandonEpoch = c()
queue = list()
frontOfLineWaits = c()

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
      if(queue[[j]]$serviceTimeWaited >= queue[[j]]$patientEpochs){
        queue[[j]] = NULL
        inc(totalAbandons)
        abandonEpoch = c(abandonEpoch, i)
        dec(totalCustomers)
        cat('happened!')
      }
    }
  }
  
  
  # Can we place someone into a slot?
  for(j in 1:numbServers) {
    # If this slot is free
    if(!serviceCompletionEpoch[j]) { 
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
        
        # Remove placed person from queue
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
  inc(Tkt[totalCustomers+1])
}
ptm2 = proc.time()
t_sim = ptm2[3] - ptm1[3]
cat("The simulation takes", t_sim, "s" )
