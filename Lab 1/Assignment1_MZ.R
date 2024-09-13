epi24 <- read.csv("~/Desktop/epi2024results06022024.csv")
View(epi24)
attach(epi24) # sets the ‘default’ object
EPI.new # prints out values EPI_data$EPI.new
tf <- is.na(EPI.new) # records True values if the value is NA
E <- EPI.new[!tf] # filters out NA values, new array

#EXERCISE 1 BELOW:
summary(EPI.new) # stats
fivenum(EPI.new,na.rm=TRUE) [1] 32.1 48.6 59.2 67.6 93.5
stem(EPI.new) # stem and leaf plot
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(EPI.new)
#Use help(<command>), e.g. > help(stem)
boxplot(EPI.new, APO.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw="SJ"))
rug(EPI.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)


#EXERCISE 2A BELOW:
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new)
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)
qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)


# EXERCISE 2A BELOW WITH MKP.new VARIABLE:
attach(epi24) # sets the ‘default’ object
MKP.new # prints out values EPI_data$EPI.new
tf <- is.na(MKP.new) # records True values if the value is NA
E <- MKP.new[!tf] # filters out NA values, new array
summary(MKP.new) # stats
fivenum(MKP.new,na.rm=TRUE) 
stem(MKP.new) # stem and leaf plot
hist(MKP.new)
hist(MKP.new, seq(0, 100))
lines(density(MKP.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(MKP.new)
#Use help(<command>), e.g. > help(stem)
boxplot(MKP.new, APO.new)
hist(MKP.new, seq(0, 100))
lines (density(MKP.new,na.rm=TRUE,bw=1.))
rug(MKP.new)
hist(MKP.new, seq(0, 100), prob=TRUE)
lines (density(MKP.new,na.rm=TRUE,bw="SJ"))
rug(MKP.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)
plot(ecdf(MKP.new), do.points=FALSE, verticals=TRUE)
qqnorm(MKP.new); qqline(MKP.new)
qqplot(rnorm(250), MKP.new, xlab = "Q-Q plot for norm dsn")
qqline(MKP.new)
qqplot(rt(250, df = 5), MKP.new, xlab = "Q-Q plot for t dsn")
qqline(MKP.new)

#plots created in exercise 1 but applied to MKP.new
summary(MKP.new) # stats
fivenum(MKP.new,na.rm=TRUE) 
stem(MKP.new) # stem and leaf plot
hist(MKP.new)
hist(MKP.new, seq(0,100), prob=TRUE)
lines(density(MKP.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(MKP.new)
#Use help(<command>), e.g. > help(stem)
boxplot(MKP.new, APO.new)
hist(MKP.new, seq(0,100), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(MKP.new)
hist(MKP.new, seq(0,100), prob=TRUE)
lines (density(MKP.new,na.rm=TRUE,bw="SJ"))
rug(MKP.new)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)



# EXERCISE 2A BELOW WITH BER.old VARIABLE:
attach(epi24) # sets the ‘default’ object
BER.old # prints out values EPI_data$EPI.new
tf <- is.na(BER.old) # records True values if the value is NA
E <- BER.old[!tf] # filters out NA values, new array
summary(BER.old) # stats
fivenum(BER.old,na.rm=TRUE) 
stem(BER.old) # stem and leaf plot
hist(BER.old)
hist(BER.old, seq(0, 100))
lines(density(BER.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(BER.old)
#Use help(<command>), e.g. > help(stem)
boxplot(BER.old, APO.new)
hist(BER.old, seq(0, 100))
lines (density(BER.old,na.rm=TRUE,bw=1.))
rug(BER.old)
hist(BER.old, seq(0, 100), prob=TRUE)
lines (density(BER.old,na.rm=TRUE,bw="SJ"))
rug(BER.old)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)
plot(ecdf(BER.old), do.points=FALSE, verticals=TRUE)
qqnorm(BER.old); qqline(BER.old)
qqplot(rnorm(250), BER.old, xlab = "Q-Q plot for norm dsn")
qqline(BER.old)
qqplot(rt(250, df = 5), BER.old, xlab = "Q-Q plot for t dsn")
qqline(BER.old)

#plots created in exercise 1 but applied to BER.old
summary(BER.old) # stats
fivenum(BER.old,na.rm=TRUE) 
stem(BER.old) # stem and leaf plot
hist(BER.old)
hist(BER.old, seq(0,100), prob=TRUE)
lines(density(BER.old,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(BER.old)
#Use help(<command>), e.g. > help(stem)
boxplot(BER.old, APO.new)
hist(BER.old, seq(0,100), prob=TRUE)
lines (density(EPI.new,na.rm=TRUE,bw=1.))
rug(BER.old)
hist(BER.old, seq(0,100), prob=TRUE)
lines (density(MKP.new,na.rm=TRUE,bw="SJ"))
rug(BER.old)
x<-seq(20,80,1)
q<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q)

