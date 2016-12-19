#### http://www.statisticsblog.com/2011/10/waiting-in-line-waiting-on-r/ ####
tmax <- 100000
t <- 0
numbServers = 40
# Total time to track
precision = 1000  #precision

ptm = proc.time()

#### NHPP Arrivals ####
get_nhpp_realization <- function(lambda){
  set.seed(10000)
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

ptm1 = proc.time()
t_np = ptm1 - ptm
cat("The non-poisson simulation takes", t_np, "s" )
# Average time each person takes at the teller, discretized exponential 
# distribution assumed Times will be augmented by one, so that everyone takes at
# least 1 interval to serve


write.table(arrivalEpochs, file = "arrivalEpochs_tmax100000_prec1000.txt", 
 row.names = F, col.names = F)
