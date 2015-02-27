# This script constrains the following topics
# 1. Link function for binary data.
# 2. Logistic regression using 0/1 response.
# 3. Logistic regression using counts data.
# 4. Variable selection and LRT
# 5. Higher order terms
# 6. Predictions for logistic regression

#----------------------------------------
# 1. Comparison of 3 link functions
#----------------------------------------

# Inverse of the link functions
p.prob <- function(x, b0, b1)
{
  pnorm(b0+b1*x, mean=0, sd=1)
}
p.logi <- function(x, b0, b1)
{
  exp(b0+b1*x)/(1+ exp(b0+b1*x))
}
p.loglog <- function(x, b0, b1)
{
  1-exp(-exp(b0+b1*x))
}

#  Plot
par(mfrow=c(2,2), lwd=2)

x<-seq(-4, 4, by=0.1)
plot(x, p.prob(x, 0, 1), type="l", main="beta0=0, beta1=1", ylab="pi")
lines(x, p.logi(x, 0, 1), lty=2, col=2)
lines(x, p.loglog(x, 0, 1), lty=3, col=3)
legend(0.5, 0.35, legend=c("Probit", "Logistic", "Log-log"), lty=c(1:3), col=c(1:3))

plot(x, p.prob(x, 0, 3), type="l", main="beta0=0, beta1=3", ylab="pi")
lines(x, p.logi(x, 0, 3), lty=2, col=2)
lines(x, p.loglog(x, 0, 3), lty=3, col=3)
legend(0.5, 0.35, legend=c("Probit", "Logistic", "Log-log"), lty=c(1:3), col=c(1:3))

plot(x, p.prob(x, 0, -1), type="l", main="beta0=0, beta1=-1", ylab="pi")
lines(x, p.logi(x, 0, -1), lty=2, col=2)
lines(x, p.loglog(x, 0, -1), lty=3, col=3)
legend(-3.5, 0.35, legend=c("Probit", "Logistic", "Log-log"), lty=c(1:3), col=c(1:3))

plot(x, p.prob(x, 1, -1), type="l", main="beta0=1, beta1=-1", ylab="pi")
lines(x, p.logi(x, 1, -1), lty=2, col=2)
lines(x, p.loglog(x, 1, -1), lty=3, col=3)
legend(-3.5, 0.35, legend=c("Probit", "Logistic", "Log-log"), lty=c(1:3), col=c(1:3))

#-------------------------------------------------
# 2. An exmaple of y=0 or 1 response in R
# Programming task example (Kunter et.al, p.565)
# 25 independent subjects,
# x: monthly experience
# y: taks success (0=failure, 1=success)
#-------------------------------------------------

prog<-read.table("ProgTask.txt", header=T)
attach(prog)
colnames(prog)
par(mfrow=c(1,2), lwd=2)
plot(Month,Success,pch=19)
boxplot(Month~Success,names=c("Failure","Success"), ylab="Experience in Month")

logisout<-glm(Success~Month, data=prog, family=binomial(link=logit))  # pay attention to the "family" statement
summary(logisout)

lowessout<-loess(Success~Month)  # Try non-parametric smoothing, just for fun
plot(Month,Success,pch=19, main="Programming Task Data")
points(Month, logisout$fitted, pch=16, col=2, cex=1.5)
points(Month, lowessout$fitted, pch=1, col=1)
legend(20, 0.2, legend=c("Observations", "Logistic Esimates", "Lowess Estimates"), pch=c(19, 16, 1), col=c(1,2,1))

detach(prog)

#-------------------------------------------------
# 3. An example of y=(# of success), i.e., y is from B(n, p)
# Beetle Mortality Data (Dobson, 7.3.1, p.127)
# y: Number of beetles killed after fives hours exposure to gaseous carbon disulphide
# n: Number of beetles 
# x: Dose
#-------------------------------------------------

beetle<-as.data.frame(matrix(c(1.6907, 59, 6,
                               1.7242, 60, 13,
                               1.7552, 62, 18,
                               1.7842, 56, 28,
                               1.8113, 63, 52,
                               1.8369, 59, 53,
                               1.861, 62, 61,
                               1.8839, 60, 60), ncol = 3, byrow=T))
colnames(beetle)<-c("dose", "n", "kill")

attach(beetle)
plot(dose, kill/n, pch=19, cex=1.5, main="Beetle Mortality", ylab="Mortality Rate")

reg.logi<-glm(kill/n~dose, family=binomial(link=logit), weight=n, data=beetle)  # 2 ways to specify the model
reg.logi2<-glm(cbind(kill, n-kill)~dose,family=binomial(link=logit) , data=beetle)  # Pay attention to the difference
summary(reg.logi)
#summary(reg.logi2) 
reg.prob<-glm(kill/n~dose, family=binomial(link=probit) , weight=n, data=beetle)
reg.cloglog<-glm(kill/n~dose, family=binomial(link=cloglog) , weight=n, data=beetle)

# plot shows cloglog is better
plot(dose, kill/n, pch=19, cex=1.5, main="Beetle Mortality", ylab="Mortality Rate")
lines(dose, reg.logi$fitted, lwd=2)
lines(dose, reg.prob$fitted, lty=2, col=2, lwd=2)
lines(dose, reg.cloglog$fitted, lty=3, col=4, lwd=2)
legend(1.7, 0.9, legend=c("Observed proportion"), pch=19, cex=1.5)
legend(1.8, 0.4, legend=c("Logistic", "Probit", "cloglog"), lty=c(1,2,3), col=c(1,2,4))

# AIC agrees
AIC(reg.logi)
AIC(reg.prob)
AIC(reg.cloglog)

detach(beetle)

#-------------------------------------------------
# 4. Variable selection and LRT for logistic regression
# Disease outbreak example (p. 684, Appendices, data set C.10)
# 196 observations (we only use the 1st half for now)
# Response variable: Disease Status, (0=no, 1=yes)
# Explanatory variables: 
#  Age (in years)
#	SES (categorical, 3 levels)
#	City Sector (categorical, 2 levels)
#	Saving account (0=no, 1=yes) (not used here)
#-------------------------------------------------

disease <- read.table("APPENC10.txt",header=F)
colnames(disease)<-c("id", "age", "ses", "sector", "status", "savings")
disease[1:10,]
cases <- 1:98  # get a "training set"
diseaseT <- disease[cases,]
attach(diseaseT)

# Use factor() to specify categorical variable.
#  Since sector only has 2 level, we don't need to treat it. (Why?) 
logit1 <- glm(status ~ age + as.factor(ses) + sector, family=binomial(link=logit), data=diseaseT)
summary(logit1) # Which level is the "control?"

# Try dummy variable and confirm the results are the same
X1 <- ses == 2
X2 <- ses == 3
logit2 <- glm(status ~ age + X1 + X2 + sector, family=binomial(link=logit))
summary(logit2)

exp(logit2$coef) # Odds ratio
cbind(age,X1,X2,sector,status,logit1$fit)

# step-wise selection using AIC
step(logit2,direction="both")
# This is equivalent
library(MASS)  
stepAIC(logit1)

# Use LRT to test if ses should be kept in the model.
logit3<-glm(status ~ age + sector, family=binomial(link=logit))
G2<-logit3$dev-logit1$dev
c(G2, 1-pchisq(G2, 2))

detach(diseaseT)

#-------------------------------------------------
# 5. Polynomial terms and interactions
#  IPO example (p.486, Appendices, data set C.11)
#  482 Initial Public Offerings companies were studied to
# 	to determine the characteristics of companies that attract venture capital
# Response variable: Venture capital funding (vcf, 0=no, 1=yes)
# Explanatory variables:
#	face value (value),
#	number of shares (num), 
# 	leveraged buyout (lev, 0=no, 1=yes)
# We only consider face value for now.
#-------------------------------------------------

ipos <- read.table("APPENC11.txt",header=F)
colnames(ipos)<-c("id", "vcf", "value", "num", "lev")
attach(ipos)
ipos[1:5,]

logv <- log(value)

par(mfrow=c(2,2), mai=c(0.5, 0.7, 0.2, 0.1))
hist(value)
box()
hist(logv)
box()
plot(value, vcf, type="p", pch=16)
plot(logv, vcf, type="p", pch=16)

lout <- loess(vcf ~ logv)
logit1 <- glm(vcf~logv,family=binomial(link=logit))
summary(logit1)
plot(logv,vcf,pch=16,ylim=c(0,1),xlab="logarithm of face 
     value",ylab="probability", main="IPOs")
lines(logv,logit1$fit,lty=1, lwd=2)
lines(logv, lout$fit, lty=3, col=3, lwd=2)
legend(14.5, 0.8, legend=c("1st-order logistic", "Loess"), lty=c(1,3), col=c(1,3))

# Try 2nd order (polynomial) fit
logvc <- logv - mean(logv)
logvc2 <- (logvc)^2
logit2 <- glm(vcf~logvc+logvc2,family=binomial(link=logit))
summary(logit2)
plot(logv,vcf,pch=16,ylim=c(0,1),xlab="logarithm of face 
     value",ylab="probability", main="IPOs")
lines(logv,logit1$fit,lty=1, lwd=2)
lines(logv, logit2$fit, lty=2, lwd=2, col=2)
lines(logv, lout$fit, lty=3, col=3, lwd=2)
legend(14.5, 0.8, legend=c("1st-order logistic", "2nd-order logistic", "Loess"),lty=c(1:3), col=c(1:3))

detach(ipos)

#----------------------------------------
# 6. Predictions for logistic regression
#----------------------------------------

# 6.1. Recall the programing task example in 2. (0-1 response)
progtask<-glm(Success~Month, data=prog, family=binomial(link=logit))  # pay attention to the "family" statement
summary(progtask)

# Fitted/predicted probabilities
progtask$fitted
fitted.values(progtask)
predict(progtask, type="response", se.fit=T)
predict(progtask, type="response", se.fit=T, newdata=data.frame(Month=c(10, 15)))

# Fitted/predicted link functions
predict(progtask)
predict(progtask, type="link", se.fit=T)
predict(progtask, type="link", se.fit=T, newdata=data.frame(Month=c(10, 15)))

# To estimate the group/class, set a cut-off value (e.g. 0.5)
progtask$fitted>0.5

# 6.2. Recall the Beetle example (counts)
beetle.logi1<-glm(kill/n~dose, family=binomial(link=logit), weight=n, data=beetle)
beetle.logi2<-glm(cbind(kill, n-kill)~dose,family=binomial(link=logit), data=beetle)
summary(beetle.logi1)
summary(beetle.logi2) 
beetle.prob<-glm(kill/n~dose, family=binomial(link=probit) , weight=n, data=beetle)
beetle.cloglog<-glm(kill/n~dose, family=binomial(link=cloglog) , weight=n, data=beetle)

# Fitted/predicted probabilities (pi_i)
beetle.logi1$fitted
fitted.values(beetle.logi2)
predict(beetle.logi1, type="response")
predict(beetle.prob, type="response")
predict(beetle.cloglog, type="response")
predict(beetle.logi1, type="response", se.fit=T, newdata=data.frame(dose=1.8))
predict(beetle.cloglog, type="response", se.fit=T, newdata=data.frame(dose=1.8))

# Predicted link functions (aka. linear prediction)
# E.g., log(pi/(1-pi)) for logit, log(-log(1-pi)) for c-loglog, etc.
predict(beetle.logi1, type="link")
predict(beetle.cloglog, type="link")
predict(beetle.logi1, type="link", se.fit=T, newdata=data.frame(dose=1.8))
predict(beetle.cloglog, type="link", se.fit=T, newdata=data.frame(dose=1.8))

# To fit/estimate the counts, we need to know the n_i
beetle.logi1$fitted*beetle$n
predict(beetle.logi1, type="response", newdata=data.frame(dose=1.8))*60

#------------------------------------------------
# This is the end of script 3
#------------------------------------------------
