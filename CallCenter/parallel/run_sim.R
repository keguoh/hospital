ptm<-proc.time()
library(parallel)
source('C:/Users/Keguo/Dropbox/GitHub/queue-systems/Callcenter/parallel/piecewise_mu.R')

ind = 1:3

(no_cores <- detectCores())
no_cl = no_cores -1
cl <- makeCluster(no_cl)
sequ <- parLapply(cl, ind, nhpp)
ptm1 = proc.time()
t_np = ptm1 - ptm
cat("The non-poisson simulation takes", t_np[3]/60, "min" )
stopCluster(cl)

len <- c()
for(i in 1:no_cl){
  len <- c(len, length(sequ[[i]][[3]]))
}
len_max = max(len)

Akt = rep(0,len_max)
Dkt = rep(0,len_max)
Tkt = rep(0,len_max)
for(i in 1:no_cl){
  Akt <- Akt + c(sequ[[i]][[3]], rep(0,len_max-len[i]))      #only takes the first 200
  Dkt <- Dkt + c(sequ[[i]][[4]], rep(0,len_max-len[i]))
  Tkt <- Tkt + c(sequ[[i]][[5]], rep(0,len_max-len[i]))
}

queueLengths <- sequ[[1]][[1]]
numbCustomers <- sequ[[1]][[2]]

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
plot(1:100, lamb[2:101])
plot(20:60, mu[20:60])
plot(1:100, mu[2:101])



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

