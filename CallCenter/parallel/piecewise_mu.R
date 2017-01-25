nhpp <- function(seed){
  set.seed(seed)
  l = 35
  b = 10/35
  g = 1
  lambda <- function(t)  l*(1+b*sin(g*t))
  precision = 1000
  t = 0
  tmax = 50  
  numbServers = 50
  numbServers_min = 30
  transitionTime = 10000
  meanServiceTime = 1
  # meanPatientTime = 1
  
  lambda_star <- function(){
    max(sapply(seq(1, tmax,length.out=1000), lambda))*2}
  X <- numeric()
  while(t <= tmax){
    u <- runif(1)
    t <- t - log(u)/lambda_star()
    if(runif(1) < lambda(t)/lambda_star()) {
      X <- c(X,t)
    }
  }
  # return(floor(X*precision))
  arrivalEpochs = floor(X*precision)
  
  epochSeq = seq(1, max(arrivalEpochs)+50, 1)
  

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
    serviceEpochsWaitedAtHeadOfQueue = 0,
    # patientEpochs = (floor(rexp(1, 1/meanPatientTime)*precision) + 1)
  )
  
  #### INITIALIZATION ####
  queueLengths = rep(20.5, length(epochSeq))
  totalCustomers = 0
  totalAbandons = 0
  numbCustomers = rep(20.5, length(epochSeq))
  serviceCompletionEpoch = rep(0, numbServers)
  waitEpoch = c()
  abandonEpoch = c()
  leavingTimes = c()
  queue = list()
  frontOfLineWaits = c()
  Akt = rep(0, length(arrivalEpochs))  #Akt, k=0,1,2,
  Dkt = rep(0, length(arrivalEpochs))  #Dkt, k=1,2,
  Tkt = rep(0, length(arrivalEpochs))  #Tkt, k=0,1,2
  
  k = 1 # counting arrival orders
  
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
        break
      }
    }
    
    
    # See if a new person is to be added to the queue
    while(i == arrivalEpochs[k] & k <= length(arrivalEpochs)) {
      inc(Akt[totalCustomers+1])
      inc(totalCustomers)
      newPerson = as.proto(person$as.list())
      newPerson$intervalArrived = i
      queue = c(queue, newPerson)
      inc(k)
    }
    
    # 
    # # If anyone is dropping the waiting line?
    # if(length(queue)) {
    #   for(j in length(queue):1) {
    #     if(queue[[j]]$serviceEpochsWaited >= queue[[j]]$patientEpochs){
    #       queue[[j]] = NULL
    #       inc(totalAbandons)
    #       abandonEpoch = c(abandonEpoch, i)
    #       inc(Dkt[totalCustomers])
    #       dec(totalCustomers)
    #       cat('happened!')
    #     }
    #   }
    # }
    
    
    # Can we place someone into a slot?
    for(j in 1:numbServers) {
      # If this slot is free
      if(!serviceCompletionEpoch[j]) { 
        if(length(queue) > 0) {
          if(j<=numbServers_min | floor(i/transitionTime)%%2==0){
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
            break
          }
        }
      }
    }
    
    # Everyone left in the queue has now waited one more interval to be served
    if(length(queue)) {
      for(j in length(queue):1) {
        inc(queue[[j]]$serviceEpochsWaited)
      }
      # The (possibley new) person at the front of the queue has had to wait 
      # there one more interval.
      inc(queue[[1]]$serviceEpochsWaitedAtHeadOfQueue)
    }
    
    
    # End of the interval, what is the state of things
    queueLengths[i] = length(queue)
    numbCustomers[i] = totalCustomers
    inc(Tkt[totalCustomers+1])
  }
  list(queueLengths, numbCustomers, Akt, Dkt, Tkt)
}