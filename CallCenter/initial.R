library(ggplot2)
jan = "C:/Users/huangke.PHIBRED/Dropbox/Research/3RD PROJECT/Callcenter/january.txt"
dat1 <- read.delim(jan)

head(dat1)
ser_time = dat1$ser_time
attach(dat1)
ggplot(dat1, aes(x=log(ser_time))) +
  geom_histogram()
min(log(ser_time))
table(ser_time==0)
detach(dat1)

nrow(dat1[dat1$outcome=="PHANTOM",])
head(dat1[dat1$type=="TT",])
table(dat1[dat1$date<990201,]$server)
table(dat1$vru.line)
