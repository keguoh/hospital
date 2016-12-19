#### http://www.statisticsblog.com/2011/10/waiting-in-line-waiting-on-r/ ####
tmax <- 10000
t <- 0
numbServers = 50
# Total time to track
precision = 1000  #precision
# epochSeq = seq(1, tmax*precision, 1)
meanServiceTime = 1
meanPatientTime = 1


ptm1 = proc.time()


# write.table(arrivalEpochs, file = "arrivalEpochs_tmax10000_prec1000.txt", 
# row.names = F, col.names = F)
arrivalEpochs <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/arrivalEpochs_tmax10000_prec1000_g1.txt",)$V1
arrivalEpochs <- arrivalEpochs
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

set.seed(100)

# Main object, really a "proto" function
person <- proto(
  intervalArrived = 0,
  # How much teller time will this person demand?
  serviceEpochsNeeded = (floor(rexp(1, 1/meanServiceTime)*precision) + 1),
  serviceEpochsWaited = 0,
  serviceEpochsWaitedAtHeadOfQueue = 0,
  patientEpochs = (floor(rexp(1, 1/meanPatientTime)*precision) + 1)
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
        if(j<=30 | floor(i/10000)%%2==0){
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
ptm2 = proc.time()
t_sim = ptm2[3] - ptm1[3]
cat("The simulation takes", t_sim, "s" )



#### Output ####
plot(queueLengths[0:25000], type="o", col="blue", pch=20, main="Queue lengths over time",
     xlab="Interval", ylab="Queue length")
plot(numbCustomers[0:25000], type="o", col="blue", pch=20, main="# of Customers over time",
     xlab="Interval", ylab="Total Customers")
# plot(totalCustomers, type="o", col="blue", pch=20, 
#      main="Total Customers over time", xlab="Person", ylab="Wait time")



# 
# max(which(Dkt != 0)) == max(which(Akt != 0))
# max(which(Dkt != 0)) == max(which(Tkt != 0)) - 1

m = max(which(Dkt != 0))
lamb = Akt[1:m]/Tkt[1:m]    # lamb_0, lamb_1 ,..., mu_(m-1)
mu = Dkt[1:m]/Tkt[2:(m+1)]  # mu_1, mu_2 ,..., mu_m

lamb
mu
plot(20:60, lamb[21:61])
plot(20:60, mu[20:60])

# lamb[is.nan(lamb)] = 0
# lamb[lamb == 0] = 0.001
# lamb[is.infinite(lamb)] = max(lamb)
# mu[is.nan(mu)] = 0
mu[mu == 0] = 0.001
# lamb[is.infinite(mu)] = max(mu)



rkt = rep(1,m+1) # r0, r1, ... ,
for(i in 2:m+1){
  rkt[i] = prod(lamb[1:(i-1)])/prod(mu[1:(i-1)])
}
rkt

alphakt = rep(0,m+1)
for (i in 1:(m+1)){
  alphakt[i] = rkt[i]/sum(rkt)
}
alphakt
plot(x=10:(m+1), y=alphakt[10:(m+1)])

#another way to modify Dkt
Dkt2 = Dkt
Dkt2[Dkt == 0] = 0.1
mu2 = Dkt2[1:m]/Tkt[2:(m+1)]  #mu_1, mu_2 ,..., mu_m
mu2
rkt2 = rep(1,m+1) # r0, r1, ... ,
for(i in 2:m+1){
  rkt2[i] = prod(lamb[1:(i-1)])/prod(mu2[1:(i-1)])
}
rkt2

alphakt2 = rep(0,m+1)
for (i in 1:(m+1)){
  alphakt2[i] = rkt2[i]/sum(rkt2)
}
alphakt2
plot(x=10:(m+1), y=alphakt2[10:(m+1)])

ptm2 = proc.time()
