#JAMES HYER SOLEY
#HOMEWORK #5 (R CODE) W/ COMMENTS
#REGRESSION (STAT 515)
#06/24/14

#QUESTION #1 (10.7; a,b)
#Part A
#IMPORT DATASET
#NAME vARIABLES
patient <- read.table("~/Math 515/Data Sets/Chapter  6 Data Sets/CH06PR15.txt", quote="\"")
names(patient)[1] <- "Y"
names(patient)[2] <- "X1"
names(patient)[3] <- "X2"
names(patient)[4] <- "X3"

#Run the following line if package "car" is not installed
#install.packages("car")

library(car)
pat.reg1<-lm(Y~X1+X2+X3, data=patient)
avPlots(pat.reg1)

#QUESTION #2 (10.11; a,b,c,d,f)
#Part A
rstudent(pat.reg1)
qt(1-(.1/(2*46)),46-4-1)

#Part B
hatvalues(pat.reg1)

#Part C
pat.reg1<-lm(Y~X1+X2+X3, data=patient, x=T)
newx<-c(1,30,58,2.0)
h<-t(newx)%*%solve(t(pat.reg1$x)%*%pat.reg1$x)%*%newx
h

#Part D
dffits(pat.reg1)
dfbeta(pat.reg1)
cooks.distance(pat.reg1)
qf(.5,4,46-4)

#Part F
plot(cooks.distance(pat.reg1))

#QUESTION #3 (10.17; a,b)
#Part A

plot(patient)
cor(patient)

#Part B

vif(pat.reg1)

#QUESTION #4 (11.7; a,c,d,e,f,g)
#IMPORT DATASET
#Part A
speed <- read.table("~/Math 515/Data Sets/Chapter 11 Data Sets/CH11PR07.txt", quote="\"")
names(speed)[1] <- "Y"
names(speed)[2] <- "X1"
speed.reg1 <- lm(Y~X1,data=speed)
plot(speed.reg1$resi~speed$X1, xlab="X1", ylab="Residuals")
speed.reg1

#Part C
plot(speed.reg1$resi^2~speed$X1, xlab="X1", ylab="Residuals^2")

#Part D
speed.reg2 <- lm(speed.reg1$resi^2~speed$X1)
speed.reg2
speed$shat1 <- lm(speed.reg1$resi^2~speed$X1)$fitted
speed$w1<-1/(speed$shat1)
speed

#Part E
speed.reg3<-lm(Y~X1, weights=w1, data=speed)
speed.reg3

#Part F
summary(speed.reg1)
summary(speed.reg3)

#Part G
speed.reg4 <- lm(speed.reg3$resi^2~speed$X1)
speed$shat2 <- lm(speed.reg3$resi^2~speed$X1)$fitted
speed$w2<-1/(speed$shat2)

speed.reg5<-lm(Y~X1, weights=w2, data=speed)
speed.reg5

summary(speed.reg3)
summary(speed.reg5)

#Question #5
copier <- read.table("~/Math 515/Data Sets/Chapter  1 Data Sets/CH01PR20.txt", quote="\"")
names(copier)[1] <- "Y"
names(copier)[2] <- "X1"
copier.reg1 <- lm(copier$Y~copier$X1)
plot(copier.reg1$resi)
dwt(copier.reg1)
