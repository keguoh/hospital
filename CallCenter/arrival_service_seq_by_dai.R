### get the arrival time and service time for Oct ###
data <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/october.txt", header = T)
head(data)
## we only need "PS" type service and q_start greater than 0 with service time greater than 0 ##
## "PS" type
q_start <- strptime(paste(data$date, data$q_start), format='%y%m%d %H:%M:%S')
data$q_start_date <- q_start
data$day_of_month <- as.numeric(format(q_start, '%d'))
data$day_of_week <- (data$day_of_month + 4) %% 7 
data$q_start_hour_of_day <- as.numeric(format(q_start, '%H'))
data$q_start_min_of_hour <- as.numeric(format(q_start, '%M'))
data$q_start_sec_of_min <- as.numeric(format(q_start, '%S'))
data$q_start_sec_of_day <- (data$q_start_hour_of_day*3600 
                            + data$q_start_min_of_hour*60
                            + data$q_start_sec_of_min)
ser_start <- strptime(paste(data$date, data$ser_start), format='%y%m%d %H:%M:%S')
data$ser_start_date <- ser_start
data$ser_start_day_of_month <- as.numeric(format(ser_start, '%d'))
data$ser_start_hour_of_day <- as.numeric(format(ser_start, '%H'))
data$ser_start_min_of_hour <- as.numeric(format(ser_start, '%M'))
data$ser_start_sec_of_min <- as.numeric(format(ser_start, '%S'))
data$ser_start_sec_of_day <- (data$ser_start_hour_of_day*3600 
                            + data$ser_start_min_of_hour*60
                            + data$ser_start_sec_of_min)

data_PS <- split(data, data$type )$PS

# valid data (positive service time or queue time)
data_valid <- data_PS[data_PS$ser_time > 0 | data_PS$q_time > 0,]
head(data_valid[,c('q_start_sec_of_day','ser_start_sec_of_day')],50)
data_valid$arrival_sec_of_day = -1
for(i in 1:nrow(data_valid)){
  if(data_valid$q_time[i] > 0){
    data_valid$arrival_sec_of_day[i] <- data_valid$q_start_sec_of_day[i] - 3600*7
  } else{data_valid$arrival_sec_of_day[i] <- data_valid$ser_start_sec_of_day[i] - 3600*7}
}


# arrival time for one day
dat1 <- data_valid[data_valid$day_of_month==1,]
arrivalEpoch <- sort(dat1$arrival_sec_of_day)
dat2 <- data_valid[data_valid$day_of_month==2,]
arrivalEpoch <- sort(dat2$arrival_sec_of_day)
dat3 <- data_valid[data_valid$day_of_month==3,]
arrivalEpochs3 <- sort(dat3$arrival_sec_of_day)

dat1 <- data_valid[data_valid$day_of_week==1,]
arrivalEpochs1 <- sort(dat1$arrival_sec_of_day)
dat_weekday <- data_valid[data_valid$day_of_week %in% c(0,3:6),]
arrivalEpochs <- sort(dat_weekday$arrival_sec_of_day)

# service time
serviceTime <- data_valid$ser_time[data_valid$ser_time > 0]
hist(serviceTime, breaks = 1000, xlim = c(0,1000))
#patient time
patientTime <- data_PS$q_time[data_PS$outcome=='HANG' & data_PS$q_time>0]
length(patientTime)
hist(patientTime, breaks=60)


#arrival Epochs
a = cut(arrivalEpochs, breaks = 3600*(0:17))
plot(7:23,table(a), type = 'o')
