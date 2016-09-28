### Research of calling center data ###
### October. data ###
data <- read.table(file = "C:\\Users\\Keguo\\Dropbox\\Research\\3RD PROJECT\\Callcenter\\october.txt", header = T)
data[1:10,]
### seperate different types of customers ###

data_PS <- split(data, data$type )$PS
data_PS_notserved <- split(data_PS, data_PS$server)$NO_SERVER
data_PS_served <- data_PS[data_PS$ser_time != "0",] 

mean_PS <- mean(data_PS_served$ser_time) #mean severice time of type PS
sd_PS <- sd(data_PS_served$ser_time)     #standard deviance of service time of type PS
SCV_PS <- sd_PS^2/mean_PS^2

barplot(table(data_PS_served$ser_time)/sum(table(data_PS_served$ser_time)), xlab = "servive time", ylab = "probability", main = "empirical density")

data_PS_served$logser_time <-log(data_PS_served[,16])
hist(data_PS_served[,18], breaks = 40, xlab = "log(servive time)", ylab = "probability", main = "empirical density")

std_service_data <- (data_PS_served[,18] - mean(data_PS_served[,18]))/sd(data_PS_served[,18]) ### standardized data
qqnorm(std_service_data) ## drawing the QQplot
abline(0,1) ## drawing a 45-degree reference line
### result is bad because of the calls with very short service time

### trancate the call data when service rate is smaller than 12
data_PS_served_adjust <- data_PS[data_PS$ser_time >= 12,] 
data_PS_served_adjust$logser_time <-log(data_PS_served_adjust[,16])
### 
hist(data_PS_served_adjust[,18],prob = T, breaks = 40,xlim = c(1,9), xlab = "adjust log(servive time)", ylab = "probability", main = "empirical density(trancated)")
## add normal density function to the histigram
x<-seq(0,10,0.01)
curve(dnorm(x, mean=mean(data_PS_served_adjust[,18]), sd=sd(data_PS_served_adjust[,18])), add=TRUE)

std_service_data_adjust <- (data_PS_served_adjust[,18] - mean(data_PS_served_adjust[,18]))/sd(data_PS_served_adjust[,18]) ### standardized data
qqnorm(std_service_data_adjust) ## drawing the QQ plot for the trancated data
abline(0,1) ## drawing a 45-degree reference line

#### getting service time ###
dat1 <- data.frame(data[1:20,])
dat1$vru_exit <- as.POSIXct(dat1$vru_exit, format = '%H:%M:%S')
dat1$vru_entry <- as.POSIXct(dat1$vru_entry, format = '%H:%M:%S')
data_difference <- dat1$vru_exit - dat1$vru_entry
