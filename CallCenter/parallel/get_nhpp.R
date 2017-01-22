ptm<-proc.time()

source('C:/Users/Keguo/Dropbox/GitHub/queue-systems/Callcenter/parallel/nhpp.R')
source('C:/Users/Keguo/Dropbox/GitHub/queue-systems/Callcenter/parallel/run_piecewise.R')

ind = 1:3

# (no_cores <- detectCores())
# cl <- makeCluster(no_cores-1)
# sequ <- parLapply(cl, ind, nhpp)
nhpp(1)
ptm1 = proc.time()
t_np = ptm1 - ptm
cat("The non-poisson simulation takes", t_np[3]/60, "min" )

run_pw <- parLapply(cl, ind, run_pw)

stopCluster(cl)

ptm2 = proc.time()
t_sim = ptm2[3] - ptm1[3]
cat("The simulation takes", t_sim/60, "min" )



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
