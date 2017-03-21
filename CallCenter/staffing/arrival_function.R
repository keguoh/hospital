dat <- read.csv('C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/staffing/Nov_Dec_data.csv', header = T)
workdat <- dat[dat$arrival_day_of_week<5, ]
#### Inital graphs ####
attach(dat)
# arrival time for one day / per hour
d1 <- dat[arrival_day_of_week==1,]
arrivalEpoch1 <- sort(d1$arrival_sec_of_day)
d2 <- dat[arrival_day_of_week==2,]
arrivalEpoch2 <- sort(d2$arrival_sec_of_day)
d3 <- dat[arrival_day_of_week==3,]
arrivalEpoch3 <- sort(d3$arrival_sec_of_day)
d4 <- dat[arrival_day_of_week==4,]
arrivalEpoch4 <- sort(d4$arrival_sec_of_day)
d5 <- dat[arrival_day_of_week==5,]
arrivalEpoch5 <- sort(d5$arrival_sec_of_day)
d6 <- dat[arrival_day_of_week==6,]
arrivalEpoch6 <- sort(d6$arrival_sec_of_day)
d0 <- dat[arrival_day_of_week==0,]
arrivalEpoch0 <- sort(d0$arrival_sec_of_day)

a1 = cut(arrivalEpoch1, breaks = 3600*(7:24))
a2 = cut(arrivalEpoch2, breaks = 3600*(7:24))
a3 = cut(arrivalEpoch3, breaks = 3600*(7:24))
a4 = cut(arrivalEpoch4, breaks = 3600*(7:24))
a5 = cut(arrivalEpoch5, breaks = 3600*(7:24))
a6 = cut(arrivalEpoch6, breaks = 3600*(7:24))
a0 = cut(arrivalEpoch0, breaks = 3600*(7:24))

plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,1200), xlim = c(7,23), 
     main = 'arrivals over hour of day')
lines(7:23+.5,as.vector(table(a1)), col=1, lwd=2)
lines(7:23+.5,as.vector(table(a2)), col=2, lwd=2)
lines(7:23+.5,as.vector(table(a3)), col=3, lwd=2)
lines(7:23+.5,as.vector(table(a4)), col=4, lwd=2)
lines(7:23+.5,as.vector(table(a5)), col=5, lwd=2)
lines(7:23+.5,as.vector(table(a6)), col=6, lwd=2)
lines(7:23+.5,as.vector(table(a0)), col=8, lwd=2)
legend('topright', as.character(1:7), col=c(1:6,8), lwd=2, cex=.75)

# 
tapply(arrivalEpoch0, paste(d0$month, d0$arrival_day_of_month), length)
tapply(arrivalEpoch1, paste(d1$month, d1$arrival_day_of_month), length)

# arrival time for one day / per half-hour
a1 = cut(arrivalEpoch1, breaks = 3600*(14:48)/2)
a2 = cut(arrivalEpoch2, breaks = 3600*(14:48)/2)
a3 = cut(arrivalEpoch3, breaks = 3600*(14:48)/2)
a4 = cut(arrivalEpoch4, breaks = 3600*(14:48)/2)
a5 = cut(arrivalEpoch5, breaks = 3600*(14:48)/2)
a6 = cut(arrivalEpoch6, breaks = 3600*(14:48)/2)
a0 = cut(arrivalEpoch0, breaks = 3600*(14:48)/2)
plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,600), xlim = c(7,23), 
     main = 'arrivals over half-hour of day')
lines((14:47)/2+.25,as.vector(table(a1)), col=1, lwd=2)
lines((14:47)/2+.25,as.vector(table(a2)), col=2, lwd=2)
lines((14:47)/2+.25,as.vector(table(a3)), col=3, lwd=2)
lines((14:47)/2+.25,as.vector(table(a4)), col=4, lwd=2)
lines((14:47)/2+.25,as.vector(table(a5)), col=5, lwd=2)
lines((14:47)/2+.25,as.vector(table(a6)), col=6, lwd=2)
lines((14:47)/2+.25,as.vector(table(a0)), col=8, lwd=2)
legend('topright', as.character(1:7), col=c(1:6,8), lwd=2, cex=.75)

# arrival time for one day / per quarter-hour
a1 = cut(arrivalEpoch1, breaks = 3600*(28:96)/4)
a2 = cut(arrivalEpoch2, breaks = 3600*(28:96)/4)
a3 = cut(arrivalEpoch3, breaks = 3600*(28:96)/4)
a4 = cut(arrivalEpoch4, breaks = 3600*(28:96)/4)
a5 = cut(arrivalEpoch5, breaks = 3600*(28:96)/4)
a6 = cut(arrivalEpoch6, breaks = 3600*(28:96)/4)
a0 = cut(arrivalEpoch0, breaks = 3600*(28:96)/4)
plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,300), xlim = c(7,23), 
     main = 'arrivals over quarter-hour of day')
lines((28:95)/4+.125,as.vector(table(a1)), col=1, lwd=2)
lines((28:95)/4+.125,as.vector(table(a2)), col=2, lwd=2)
lines((28:95)/4+.125,as.vector(table(a3)), col=3, lwd=2)
lines((28:95)/4+.125,as.vector(table(a4)), col=4, lwd=2)
lines((28:95)/4+.125,as.vector(table(a5)), col=5, lwd=2)
lines((28:95)/4+.125,as.vector(table(a6)), col=6, lwd=2)
lines((28:95)/4+.125,as.vector(table(a0)), col=8, lwd=2)
legend('topright', as.character(1:7), col=c(1:6,8), lwd=2, cex=.75)
detach(dat)

attach(workdat)
# three cases for workdays
a = cut(workdat$arrival_sec_of_day, breaks = 3600*(28:96)/4)
plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,1400), xlim = c(7,23), 
     main = 'arrivals over quarter-hour of day (workdays)')
lines((28:95)/4+.125,as.vector(table(a)), col=1, lwd=2)

a = cut(workdat$arrival_sec_of_day, breaks = 3600*(14:48)/2)
plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,2800), xlim = c(7,23), 
     main = 'arrivals over half-hour of day (workdays)')
lines((14:47)/2+.25,as.vector(table(a)), col=1, lwd=2)

a = cut(workdat$arrival_sec_of_day, breaks = 3600*(7:24))
plot(0, ylab = "# of arrivals", xlab = "hours of day", type = 'n', 
     ylim = c(0,5600), xlim = c(7,23), 
     main = 'arrivals over hour of day (workdays)')
lines((7:23)+.5,as.vector(table(a)), col=1, lwd=2)


attach(workdat)
#### fit function ####
X = aggregate(arrival_day_of_month, by=list(paste(month,arrival_day_of_month), arrival_day_of_week,arrival_hour_of_day), length)
colnames(X) <- c('day', 'weekday', 'hour', 'arrivals')
X$hour <- X$hour+.5
X$hour2 <- (X$hour)^2
X$hour3 <- (X$hour)^3
X$hour4 <- (X$hour)^4
X$hour5 <- (X$hour)^5
X <- X[order(X$day), ]

#### polynomials ####
X1 <- X[X$weekday==1,]
head(X1)
plot(0, xlim = c(7,24), ylim = c(min(X1$arrivals), max(X1$arrivals)), 
     main = 'polynomial for arrival rate function (MONDAYS)',
     xlab = "Hours", ylab = '# of Arrivals')
for(i in 1:(nrow(X1)/17)){
  lines(X1$hour[(i*17-16):(i*17)], X1$arrivals[(i*17-16):(i*17)])
}

o1 <- lm(arrivals ~ 0 + factor(weekday) + hour:factor(weekday), data = X)
b1 <- o1$coefficients
abline(b1[2], b1[7], col=2,lwd=2)
o2 <- lm(arrivals ~ 0 + factor(weekday) + hour:factor(weekday) 
         + hour2:factor(weekday), data = X)
b2 <- o2$coefficients
x <- seq(7,24,by=.01)
y <- b2[2] + b2[7]*x + b2[12]*x^2
lines(x,y,col=3,lwd=2)
o3 <- lm(arrivals ~ 0 + factor(weekday) + hour:factor(weekday) 
         + hour2:factor(weekday) + hour3:factor(weekday), data = X)
b3 <- o3$coefficients
x <- seq(7,24,by=.01)
y <- b3[2] + b3[7]*x + b3[12]*x^2 + b3[17]*x^3
lines(x,y,col=4,lwd=2)
o4 <- lm(arrivals ~ 0 + factor(weekday) + hour:factor(weekday) 
         + hour2:factor(weekday) + hour3:factor(weekday) + hour4:factor(weekday), data = X)
b4 <- o4$coefficients
x <- seq(7,24,by=.01)
y <- b4[2] + b4[7]*x + b4[12]*x^2 + b4[17]*x^3 + b4[22]*x^4
lines(x,y,col=5,lwd=2)
o5 <- lm(arrivals ~ 0 + factor(weekday) + hour:factor(weekday) 
         + hour2:factor(weekday) + hour3:factor(weekday)
         + hour4:factor(weekday) + hour5:factor(weekday), data = X)
b5 <- o5$coefficients
x <- seq(7,24,by=.01)
y <- b5[2] + b5[7]*x + b5[12]*x^2 + b5[17]*x^3 + b5[22]*x^4 + b5[27]*x^5
lines(x,y,col=6,lwd=2)
legend('topright',as.character(1:5), title = 'order', col = 2:6, lwd=2)


attach(workdat)
#### mixed normal ####
X1 <- X[X$weekday==1,]
head(X1)
plot(0, xlim = c(7,24), ylim = c(min(X1$arrivals), max(X1$arrivals)), 
     main = 'two-normal for arrival rate function (MONDAYS)',
     xlab = "Hours", ylab = '# of Arrivals')
for(i in 1:(nrow(X1)/17)){
  lines(X1$hour[(i*17-16):(i*17)], X1$arrivals[(i*17-16):(i*17)])
}

library(minpack.lm)
o <- nlsLM(arrivals ~ 1/sqrt(2*pi)/s1*exp(-(hour-mu1)^2/(2*s1^2)) 
         + 1/sqrt(2*pi)/s2*exp(-(hour-mu2)^2/(2*s2^2)), data = X,
         start=list(mu1=10, s1=1, mu2=15, s2=2), 
         control = nls.control(maxiter = 100), trace=TRUE)
b = coef(o)
x <- seq(7,24,by=.01)
y1 <- 1/sqrt(2*pi*b[2])*exp(-(x-b[1])^2/2/b[2])
y2 <- 1/sqrt(2*pi*b[4])*exp(-(x-b[3])^2/2/b[4])
lines(x,y1,col=3,lwd=2)
