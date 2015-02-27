# This script discusses the classfication and validation in logistic
# regression.  R script to prepare ROC (Receiver Operating Characteristic)
# curve is provided

#---------------------------------------------
# Data sets
#--------------------------------------------

# Data: Recall the Disease outbreak example
# (Kutner, p. 684, Appendices, data set C.10)
# 196 observations (we only use the 1st half for now)
# Response variable: Disease Status, (0=no, 1=yes)
# Explanatory variables: 
#	Age (in years)
#     SES (categorical, 3 levels)
#	City Sector (categorical, 2 levels)
#	Saving account (0=no, 1=yes) (not used here)

disease <- read.table("APPENC10.txt",header=F)
colnames(disease)<-c("id", "age", "ses", "sector", "status", "savings")
cases <- 1:98  # get a "training set"
diseaseT <- disease[cases,]

logit <- glm(status ~ age + as.factor(ses) + sector,
             family=binomial(link=logit), data=diseaseT)

# Classification table at a given C (consider c=0.325 to minimize overall error rate)
#c1<-0.318
c1<-0.325
pi<-logit$fitted
ypred<-1*(pi>=c1)
table(diseaseT$status, ypred)
fp<-sum((ypred==1) & (diseaseT$status==0))/sum(diseaseT$status==0)
fn<-sum((ypred==0) & (diseaseT$status==1))/sum(diseaseT$status==1)
erate<-(sum((ypred==1) & (diseaseT$status==0))+
          sum((ypred==0) & (diseaseT$status==1)))/length(diseaseT$status)
c(fp, fn, erate)

# Validation set
diseaseV<-disease[-cases,]
pi2<-predict(logit, diseaseV, type="response")
ypred2<-1*(pi2>=c1)
table(diseaseV$status, ypred2)
fp2<-sum((ypred2==1) & (diseaseV$status==0))/sum(diseaseV$status==0)
fn2<-sum((ypred2==0) & (diseaseV$status==1))/sum(diseaseV$status==1)
erate2<-(sum((ypred2==1) & (diseaseV$status==0))+
           sum((ypred2==0) & (diseaseV$status==1)))/length(diseaseV$status)
c(fp2, fn2, erate2)

# The following roc analysis choses optimal C to max(sensitivity+specificity)
roc.analysis <-function (object, newdata = NULL, newplot=TRUE) 
{
  if (is.null(newdata)) {
    pi.tp <- object$fitted[object$y == 1]
    pi.tn <- object$fitted[object$y == 0]
  }
  else {
    pi.tp <- predict(object, newdata, type = "response")[newdata$y == 1]
    pi.tn <- predict(object, newdata, type = "response")[newdata$y == 0]
  }
  
  pi.all <- sort(c(pi.tp, pi.tn))
  sens <- rep(1, length(pi.all)+1)
  specc <- rep(1, length(pi.all)+1)
  for (i in 1:length(pi.all)) {
    sens[i+1] <- mean(pi.tp >= pi.all[i], na.rm = T)
    specc[i+1] <- mean(pi.tn >= pi.all[i], na.rm = T)
  } 
  
  npoints <- length(sens)
  area <- sum(0.5 * (sens[-1] + sens[-npoints]) * (specc[-npoints] - 
                                                     specc[-1]))
  lift <- (sens - specc)[-1]
  cutoff <- pi.all[lift == max(lift)][1]
  sensopt <- sens[-1][lift == max(lift)][1]
  specopt <- 1 - specc[-1][lift == max(lift)][1]
  
  if (newplot){
    plot(specc, sens, xlim = c(0, 1), ylim = c(0, 1), type = "s", 
         xlab = "1-specificity", ylab = "sensitivity", main="ROC")
    abline(0, 1)
  }
  else lines(specc, sens, type="s", lty=2, col=2)
  
  list(pihat=as.vector(pi.all), sens=as.vector(sens[-1]), 
       spec=as.vector(1-specc[-1]), area = area, cutoff = cutoff,
       sensopt = sensopt, specopt = specopt)
}

trainingROC<-roc.analysis(logit)
trainingROC

diseaseV$y<-diseaseV$status
validationROC<-roc.analysis(logit, newdata=diseaseV, newplot=F)
validationROC

#------------------------------------------------
# This is the end of script 5.R
#------------------------------------------------
