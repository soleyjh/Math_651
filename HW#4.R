#JAMES HYER SOLEY
#HOMEWORK #3 (R CODE) W/ COMMENTS
#REGRESSION (STAT 515)
#06/17/14

#QUESTION #1 (8.4; a,b,c,d,e,f,g)
#8.4 Part A

muscle <- read.table("~/Math 515/Data Sets/Chapter  1 Data Sets/CH01PR27.txt", quote="\"")
names(muscle)[1] <- "Y"
names(muscle)[2] <- "X1"
summary(muscle)
plot(Y~X1, data=muscle)

muscle$x1 <- (muscle$X1 - mean(muscle$X1))
muscle.reg2<- lm(Y~x1+I(x1^2), data=muscle)
summary(muscle.reg2)
plot(Y~X1, data=muscle)
lines(muscle$X1,fitted(lm(muscle$Y~muscle$x1+I(muscle$x1^2))))

#8.4 Part B

anova(muscle.reg2)

#8.4 Part C

y <- 48-mean(muscle$X1)
predict(muscle.reg2, data.frame(x1= y), se.fit = T, interval = "confidence", level=.95)

#8.4 Part D

predict(muscle.reg2, data.frame(x1= y), se.fit = T, interval = "prediction", level=.95)

#8.4 Part E

anova(muscle.reg2)

#8.4 Part F

muscle.reg1<- lm(Y~X1+I(X1^2), data=muscle)
summary(muscle.reg1)

#8.4 Part G

cor(muscle$X1,muscle$X1^2)
cor(muscle$x1,muscle$x1^2)

#QUESTION #2 (8.25; a,b)
#8.25 Part A
grocery <- read.delim("~/Math 515/Data Sets/Chapter  6 Data Sets/CH06PR09.txt", header=F)
names(grocery)[1] <- "Y"
names(grocery)[2] <- "X1"
names(grocery)[3] <- "X2"
names(grocery)[4] <- "X3"

grocery$x1 <- grocery$X1-mean(grocery$X1)
groc.reg1 <- lm(Y~x1+I(x1^2)+X3+x1:X3+I(x1^2):X3,data=grocery)
summary(groc.reg1)

#8.25 Part B
#Case 2
groc.reg3 <- lm(Y~x1+X3+x1:X3,data=grocery)
anova(groc.reg3)

qf(.95,2,46)
qf(.95,3,46)

#QUESTION #3 (8.39; a,b,c)
#8.39 Part A

cdi <- read.table("~/Math 515/Data Sets/Appendix C Data Sets/APPENC02.txt", quote="\"")
names(cdi)[8] <- "Y"
names(cdi)[5] <- "X1"
names(cdi)[16] <- "X2"
names(cdi)[17] <- "Region"
cdi$X3 <- 0
cdi$X3[cdi$Region==1] <- 1
cdi$X4 <- 0
cdi$X4[cdi$Region==2] <- 1
cdi$X5 <- 0
cdi$X5[cdi$Region==3] <- 1
cdi$X6 <- 0
cdi$X6[cdi$Region==4] <- 1
cdi.reg1 <- lm(Y~X1 + X2 + X3 + X5 + X6, data=cdi)
cdi.reg1

confint(cdi.reg1, level=.90)

cdi.reg3 <- lm(Y~X1 + X2 + X3, data=cdi)
cdi.reg3
confint(cdi.reg3)


#8.39 Part B

summary(cdi.reg1)

#8.39 Part C

anova(cdi.reg1)

# QUESTION #4 (9.18; a,b)
# 9.18 Part A

job <- read.table("~/Math 515/Data Sets/Chapter  9 Data Sets/CH09PR10.txt", quote="\"")
names(job)[1] <- "Y"
names(job)[2] <- "X1"
names(job)[3] <- "X2"
names(job)[4] <- "X3"
names(job)[5] <- "X4"

#PROFILE DATA
#install.packages("leaps")
summary(job)

# build selcri function
selcri<-function(lmout)
{
  n <- length(lmout$fit)
  rsq <- summary(lmout)$r.sq
  adj.rsq <- summary(lmout)$adj.r.sq
  aic <- extractAIC(lmout)[2]
  bic <- extractAIC(lmout, k = log(n))[2]
  press <- sum((lmout$residuals/(1 - hatvalues(lmout)))^2)
  cbind(rsq, adj.rsq, aic, bic, press)
}

job.reg1<-lm(Y ~ X1, data=job)
job.reg2<-lm(Y ~ X1 + X2 + X3 + X4 , data=job)

# Stepwise selection based on aic (or bic)
step(job.reg2)

# 9.18 Part B
# Best Subset selection (we need to install a package: leaps)
# Run the following line if package "leaps" is not installed
# Installed above

job.best1<-regsubsets(Y ~ X1 + X2 + X3 + X4, data=job)
bestsub<-summary(job.best1)
bestsub
bestsub$adjr2
bestsub$rsq
bestsub$bic
bestsub$cp

selcri(job.reg2)