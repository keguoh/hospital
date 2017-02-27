### get the arrival time and service time for Oct ###
data <- read.table(file = "C:/Users/Keguo/Dropbox/GitHub/queue-systems/CallCenter/october.txt", header = T)
data[1:10,]
## we only need "PS" type service and q_start greater than 0 with service time greater than 0 ##
## "PS" type
data_PS <- split(data, data$type )$PS
q.need <- data_PS[,c(6,10,14,16)]
head(q.need)
q.start <- strptime(paste(data_PS$date, q.need$q_start), format='%y%m%d %H:%M:%S')
q.index <- rep(0, length(q.start))
## get the one hour function ###
Oct1 <- strptime("991001 00:00:00", format='%y%m%d %H:%M:%S')
hrs <- function(u) {
  x <- u * 3600
  return(x)
}
## q_start between 7-24 ###
for(i in 1:length(q.start)){
  for (k in 1:31){
    if(q.start[i] < Oct1 + hrs(24*k-17) && q.start[i] >= Oct1 + hrs(24*k-24) - 1) {q.index[i] <- i}}
}
q.need2 <- q.need[-q.index,]
## service time should greater than 0 ###
q.need.new <- q.need2[q.need2$ser_time > 0,3:4]
head(q.need.new,20)
