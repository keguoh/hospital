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
length(res_1)
hist(res_1)


bs <- c(0.01, 0.1, 1)
b <- bs[1]
lambda <- function(t)  l*(1+b*sin(g*t))
res_1 <- get_nhpp_realization(lambda)
n_1 <- length(res_1)
b <- bs[2]
lambda <- function(t) b*t^2
res_2 <- get_nhpp_realization(lambda)
n_2 <- length(res_2)
b <- bs[3]
lambda <- function(t) b*t^2
res_3 <- get_nhpp_realization(lambda)
n_3 <- length(res_3)

plot(stepfun(res_1,c(0:length(res_1))),xlim = c(0,10),do.points = F,main="L=0.5")
plot(stepfun(res_2,c(0:length(res_2))),xlim = c(0,10),do.points = F,main="L=0.5")
plot(stepfun(res_3,c(0:length(res_3))),xlim = c(0,10),do.points = F,main="L=0.5")
