# Data 1: Recall the Disease outbreak example
# (Kutner, p. 684, Appendices, data set C.10)
# 196 observations (we only use the 1st half for now)
# Response variable: Disease Status, (0=no, 1=yes)
# Explanatory variables: 
#  Age (in years)
#     SES (categorical, 3 levels)
#	City Sector (categorical, 2 levels)
#	Saving account (0=no, 1=yes) (not used here)

disease <- read.table("APPENC10.txt",header=F)
colnames(disease)<-c("id", "age", "ses", "sector", "status", "savings")
cases <- 1:98  # get a "training set"
diseaseT <- disease[cases,]

# Data 2: Revisit the Beetle Mortality Data (Dobson)
# y: Number of beetles killed after fives hours exposure to
#    gaseous carbon disulphide
# n: Number of beetles 
# x: Dose

beetle<-as.data.frame(matrix(c(1.6907, 59, 6,
                               1.7242, 60, 13,
                               1.7552, 62, 18,
                               1.7842, 56, 28,
                               1.8113, 63, 52,
                               1.8369, 59, 53,
                               1.861, 62, 61,
                               1.8839, 60, 60), ncol = 3, byrow=T))
colnames(beetle)<-c("dose", "n", "kill")

#----------------------------------------
# 1. Residuals and residual plots
#----------------------------------------

# 1.1. Disease outbreak data

disease.logit <- glm(status ~ age + as.factor(ses) + sector,
                     family=binomial(link=logit), data=diseaseT, x=T)

# Different predictions
pi <- disease.logit$fitted  # Estimated probability
lp <- predict(disease.logit, type="link")  # Linear prediction: log(pi/(1-pi))

# Get Hat matrix for Logistic Regression
W <- diag(pi*(1-pi))
X<-disease.logit$x
H<-sqrt(W)%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%sqrt(W)
h <- diag(H)

# Get different residuals
e <- diseaseT$status-pi  # Oridinary residual
rp<- e/sqrt(pi*(1-pi))  # Pearson Residual
rsp <- rp/sqrt(1-h)  # Studentized Pearson residual
dev <- residuals(disease.logit)  # Deviance residual
diseaseT.out <- cbind(diseaseT$status,pi,lp, e,rp,rsp,dev,h)

round(diseaseT.out[1:10,], 4)

# Prepare graphs
par(mfrow=c(2,2), las=1)
indi <- sort(pi,index.return=T)$ix
loe<-loess(rp ~ pi, degree=1)
plot(pi, rp, type="p", pch=16, xlab="Est. Prob.", ylab="Pearson Residual")
lines(pi[indi], loe$fitted[indi], type="l")
loe<-loess(rp ~ lp, degree=1)
plot(lp, rp, type="p", pch=16, xlab="Est. Link.", ylab="Pearson Residual")
lines(lp[indi], loe$fitted[indi], type="l")

loe<-loess(dev ~ pi, degree=1)
plot(pi, dev, type="p", pch=16, xlab="Est. Prob.", ylab="Deviance Residual")
lines(pi[indi], loe$fitted[indi], type="l")

loe<-loess(dev ~ lp, degree=1)
plot(lp, dev, type="p", pch=16, xlab="Est. Link.", ylab="Deviance Residual")
lines(lp[indi], loe$fitted[indi], type="l")

# 1.2 Beetle data (counts)

plot(beetle$dose, beetle$kill/beetle$n, pch=19, cex=1.5, main="Beetle Mortality", ylab="Mortality Rate")

beetle.logit<-glm(kill/n~dose, family=binomial(link=logit), weight=n,
                  data=beetle)

# Predictions and residuals
pi <- beetle.logit$fitted  
lp <- predict(beetle.logit, type="link")
yhat<-beetle$n*pi
e <- beetle$kill-yhat  # Oridinary residual
rp<- e/sqrt(beetle$n*pi*(1-pi))  # Pearson Residual
dev <- residuals(beetle.logit)  # Deviance residual
beetle.res1 <- cbind(beetle$kill,beetle$yhat1, pi,lp, e,rp,dev)
round(beetle.res1, 4)

# Prepare graphs
par(mfrow=c(2,2), las=1)
indi <- sort(pi,index.return=T)$ix
loe<-loess(rp ~ pi, degree=1)
plot(pi, rp, type="p", pch=16, xlab="Est. Prob.", ylab="Pearson Residual", main="Logistic Link")
lines(pi[indi], loe$fitted[indi], type="l")
loe<-loess(dev ~ pi, degree=1)
plot(pi, dev, type="p", pch=16, xlab="Est. Prob.", ylab="Deviance Residual", main="Logistic Link")
lines(pi[indi], loe$fitted[indi], type="l")

#  Try a c-log-log link
beetle.clog<-glm(kill/n~dose, family=binomial(link=cloglog), weight=n, data=beetle)
pi <- beetle.clog$fitted  # Estimated probability
yhat<-beetle$n*pi
e <- beetle$kill-yhat  # Oridinary residual
rp<- e/sqrt(beetle$n*pi*(1-pi))  # Pearson Residual
dev <- residuals(beetle.clog)  # Deviance residual
indi <- sort(pi,index.return=T)$ix
loe<-loess(rp ~ pi, degree=1)
plot(pi, rp, type="p", pch=16, xlab="Est. Prob.", ylab="Pearson Residual", main="C-LogLog Link")
lines(pi[indi], loe$fitted[indi], type="l")
loe<-loess(dev ~ pi, degree=1)
plot(pi, dev, type="p", pch=16, xlab="Est. Prob.", ylab="Deviance Residual", main="C-LogLog Link")
lines(pi[indi], loe$fitted[indi], type="l")

#----------------------------------------
# 2. Outliers and influential cases
# Use the disease outbreak example
#----------------------------------------

# Get leverages so that we can comput Cook's D and dDev.
disease.logit
pi<-disease.logit$fitted
W <- diag(pi*(1-pi))
X<-disease.logit$x
H<-sqrt(W)%*%X%*%solve(t(X)%*%W%*%X)%*%t(X)%*%sqrt(W)
h <- diag(H)

# Cook's Distance 
rp<- (diseaseT$status-pi)/sqrt(pi*(1-pi))  # Pearson Residual
D<-rp^2*(h)/(5*(1-h)^2) # Recall we have 5 parameters

# Change in Pearson Chi-square
rsp <- rp/sqrt(1-h)  # Studentized Pearson residual
dChi<-rsp^2

# Change in Deviance
dev<-residuals(disease.logit)
ddev<-h*rsp^2+dev^2

diseaseT.infl<-cbind(rp, rsp, h, dev, dChi, ddev, D)
round(diseaseT.infl[c(1:3, 96:98),], 3) # Table 14.11 on p.599 in Kunter et.al

# graphical display
par(mfrow=c(2,2), las=1)
# Leverage
plot(h, main="Leverage", pch=16, ylab="", xlab="") # look for large values
abline(2*5/98, 0, lty=2,lwd=2, col=2)
lv<-h>2*5/98
text(c(1:length(h))[lv], h[lv], labels=c(1:length(h))[lv], pos=1)
# Cook's Distance
plot(D, main="Cook's Distance", ylab="", xlab="", pch=16) # Look for spikes/large values
lv<-D>0.06
text(c(1:length(D))[lv], D[lv], labels=c(1:length(h))[lv], pos=1)
# Change in Chi-square
plot(dChi, main="Changes in Chi-square", pch=16, ylab="", xlab="")
lv<-dChi>7
text(c(1:length(dChi))[lv], dChi[lv], labels=c(1:length(dChi))[lv], pos=1)
# Change in Deviance
plot(ddev, main="Changes in Deviance", pch=16, ylab="", xlab="")
lv<-ddev>4
text(c(1:length(ddev))[lv], ddev[lv], labels=c(1:length(ddev))[lv], pos=1)

# Plot against pi, and use D to decide the size
# Look for upper corners with large D
# argument cex= controls the size the circle.  Different values can be used as 
# long as we can see the difference.
par(mfrow=c(1,2), las=1)
lv<-D>0.06
plot(pi, dChi, cex=exp(D*10), main="Change in Chi-square vs. Pi vs. D", ylab="dChi")
text(pi[lv], dChi[lv], labels=c(1:length(pi))[lv], pos=1)
plot(pi, ddev, cex=sqrt(D)*10, main="Change in Deviance vs. Pi vs. D", ylab="dDev")
text(pi[lv], ddev[lv], labels=c(1:length(pi))[lv], pos=1)

# In addition, check out glm.diag and glm.diag.plot in package "boot."

#----------------------------------------
# 3. Goodness of fit tests
#----------------------------------------

# 3.1. Hosmer-Lemeshow test (Appropriate for 0-1 responses)
# The following function is addopted from
# http://www.math.mcmaster.ca/peter/s4f03/s4f03_0607/rochl.html
hosmerlem<-function(y, yhat, g = 10)
{
  cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 1, 1/g)), include.lowest = T)
  obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2/expect)
  P <- 1 - pchisq(chisq, g - 2)
  c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
} 
# Recall the disease outbreak example
pi <- disease.logit$fitted
hosmerlem(diseaseT$status, pi, g=10)

# 3.2. Pearson Chi-square (counts data only)
pgof<-function(n, y, pihat, p)
{
  Oy<-y
  On<-n-y
  Ey<-n*pihat
  En<-n*(1-pihat)
  chisq<-sum((Oy-Ey)^2/Ey) + sum((On-En)^2/En)
  pvalue <- 1 - pchisq(chisq, length(n)-p)
  c("X^2" = chisq, Df = length(n)-p, "P(>Chi)" = pvalue)
}
# Use Beetle Mortality data again
pgof(beetle$n, beetle$kill, beetle.logit$fitted, 2)
pgof(beetle$n, beetle$kill, beetle.clog$fitted, 2)

# 3.3. Deviance test (counts data only)
c(beetle.logit$dev, sum(residuals(beetle.logit)^2))  # just to show they are the same
1-pchisq(beetle.logit$dev, df=length(beetle$dose)-2) # why -2?
1-pchisq(beetle.clog$dev, df=length(beetle$dose)-2) # why -2?

#------------------------------------------------
# This is the end of script 4
#------------------------------------------------
