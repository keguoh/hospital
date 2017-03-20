### get the arrival time and service time for Nov/Dec ###
dat1 <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/november.txt", header = T)
q_start1 <- strptime(paste(dat1$date, dat1$q_start), format='%y%m%d %H:%M:%S')
dat1$arrival_day_of_month <- as.numeric(format(q_start1, '%d'))
dat1$arrival_day_of_week <- (dat1$arrival_day_of_month) %% 7 
dat1$month <- 11

dat2 <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/december.txt", header = T)
q_start2 <- strptime(paste(dat2$date, dat2$q_start), format='%y%m%d %H:%M:%S')
dat2$arrival_day_of_month <- as.numeric(format(q_start2, '%d'))
dat2$arrival_day_of_week <- (dat2$arrival_day_of_month + 2) %% 7 
dat2$month <- 12
dat <- rbind(dat1, dat2)
## we only need "PS" type service and q_start greater than 0 with service time greater than 0 ##
## "PS" type
dat <- split(dat, dat$type )$PS
dat <- dat[dat$ser_time > 0 | dat$q_time > 0,]


q_start <- strptime(paste(dat$date, dat$q_start), format='%y%m%d %H:%M:%S')
q_start_hour_of_day <- as.numeric(format(q_start, '%H'))
q_start_min_of_hour <- as.numeric(format(q_start, '%M'))
q_start_sec_of_min <- as.numeric(format(q_start, '%S'))
q_start_sec_of_day <- (q_start_hour_of_day*3600 
                            + q_start_min_of_hour*60
                            + q_start_sec_of_min)
ser_start <- strptime(paste(dat$date, dat$ser_start), format='%y%m%d %H:%M:%S')
ser_start_hour_of_day <- as.numeric(format(ser_start, '%H'))
ser_start_min_of_hour <- as.numeric(format(ser_start, '%M'))
ser_start_sec_of_min <- as.numeric(format(ser_start, '%S'))
ser_start_sec_of_day <- (ser_start_hour_of_day*3600 
                            + ser_start_min_of_hour*60
                            + ser_start_sec_of_min)


# valid dat (positive service time or queue time)
dat$arrival_sec_of_day = -1
for(i in 1:nrow(dat)){
  if(dat$q_time[i] > 0){
    dat$arrival_sec_of_day[i] <- q_start_sec_of_day[i]
  } else{dat$arrival_sec_of_day[i] <- ser_start_sec_of_day[i]}
}
dat$arrival_min_of_hour = dat$arrival_sec_of_day%/%60%%60
dat$arrival_hour_of_day = dat$arrival_sec_of_day%/%(60*60)

# delete some data after midnight and before 7 am
dat <- dat[dat$arrival_sec_of_day >= 7*60*60, ]
write.csv(dat, file='Nov_Dec_data.csv')


# service time
serviceTime <- dat$ser_time[dat$ser_time > 0]
length(serviceTime)
hist(serviceTime, breaks = 1000, xlim = c(0,1000))

#patient time
patientTime <- dat$q_time[dat$outcome=='HANG' & dat$q_time>0]
length(patientTime)
hist(patientTime, breaks=60)

# arrival time for one day / per hour
d1 <- dat[dat$arrival_day_of_week==1,]
arrivalEpoch1 <- sort(d1$arrival_sec_of_day)
d2 <- dat[dat$arrival_day_of_week==2,]
arrivalEpoch2 <- sort(d2$arrival_sec_of_day)
d3 <- dat[dat$arrival_day_of_week==3,]
arrivalEpoch3 <- sort(d3$arrival_sec_of_day)
d4 <- dat[dat$arrival_day_of_week==4,]
arrivalEpoch4 <- sort(d4$arrival_sec_of_day)
d5 <- dat[dat$arrival_day_of_week==5,]
arrivalEpoch5 <- sort(d5$arrival_sec_of_day)
d6 <- dat[dat$arrival_day_of_week==6,]
arrivalEpoch6 <- sort(d6$arrival_sec_of_day)
d0 <- dat[dat$arrival_day_of_week==0,]
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
