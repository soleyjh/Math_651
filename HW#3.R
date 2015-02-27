#JAMES HYER SOLEY
#HOMEWORK #3 (R CODE) W/ COMMENTS
#REGRESSION (STAT 515)
#06/12/14

#QUESTION #1 (6.9; a,b,c)

#IMPORT DATASET
#NAME vARIABLES
Grocery <- read.delim("~/Math 515/Data Sets/Chapter  6 Data Sets/CH06PR09.txt", header=F)
names(Grocery)[1] <- "Y"
names(Grocery)[2] <- "X1"
names(Grocery)[3] <- "X2"
names(Grocery)[4] <- "X3"

#6.9 Part A
stem(Grocery$X1)
hist(Grocery$X1)
stem(Grocery$X2)
hist(Grocery$X2)

#6.9 Part B
Grocery$Week <- as.numeric(row.names(Grocery))
plot(Grocery$Week,Grocery$X1)
plot(Grocery$Week,Grocery$X2)
plot(Grocery$Week,Grocery$X3)

#6.9 Part C
Grocery$Week <- NULL
cor(Grocery)
plot(Grocery)

#QUESTION #2 (6.10; a,b,c,d)
#6.10 Part A
groc_reg1<-lm(Y~X1+X2+X3, data=Grocery)
summary(groc_reg1)

#6.10 Part B
groc_reg1$resi
boxplot(groc_reg1$resi)

#Create new Variable X1*X2 and Plot
Grocery$X1_X2 <- Grocery$X1*Grocery$X2
plot(groc_reg1$resi~Grocery$X1_X2)

#Standardize the Regression Scores
grocery.stdres <- rstandard(groc_reg1)
qqnorm(grocery.stdres)
qqline(grocery.stdres)
grocery.stdres

#6.10 Part D
Grocery$Week <- as.numeric(row.names(Grocery))
plot(Grocery$Week,groc_reg1$resi)

#QUESTION #3 (6.11; a,b,c)
#6.11 Part A

anova(groc_reg1)
((136366+5726+2034514) / 3)/20532
qf(.95,3,48)
1 - pf(35.33681,3,48)

#6.11 Part B
summary(groc_reg1)
B <- qt(.9875,48)
b1_est <- .0007871
b3_est <- 623.6
b1_se <- .0003646
b3_se <- 62.64

#CI
b1_est - B*b1_se
b1_est + B*b1_se
b3_est - B*b3_se
b3_est + B*b3_se

#6.11 Part C
anova(groc_reg1)
SSR <- (136366+5726+2034514)
SSE <- 985530
R_SQR <- SSR/(SSR+SSE)
R_SQR

#QUESTION #4 (6.12; a,b)
#6.12 Part A

W <- sqrt(4*qf(.95,4,48))
W
B <- qt(.995,48)
B

new <- data.frame(X1=c(302000,245000,280000,350000,295000),X2=c(7.2,7.4,6.9,7.0,6.7),X3=c(0,0,0,0,1))
predict(groc_reg1, new, se.fit = T, interval = "confidence", level=(1-.1/5))

one_est <- 4150 + (.0007871*302000) + (-13.17*7.20) + (623.6 *0)
two_est <- 4150 + (.0007871*245000) + (-13.17*7.40) + (623.6 *0)
three_est <- 4150 + (.0007871*280000) + (-13.17*6.90) + (623.6 *0)
four_est <- 4150 + (.0007871*350000) + (-13.17*7.0) + (623.6 *0)
five_est <- 4150 + (.0007871*295000) + (-13.17*6.7) + (623.6 *1)

#CI
one_est - B*21.3567
one_est + B*21.3567
two_est - B*29.7021
two_est + B*29.7021
three_est - B*24.4444
three_est + B*24.4444
four_est - B*28.9293
four_est + B*28.9293
five_est - B*62.4998
five_est + B*62.4998

#6.12 Part B
plot(Grocery$X1~Grocery$X2)

#QUESTION #5 (6.13; a)
#6.13 Part A

S <- sqrt(4*qf(.95,4,48))
S
B <- qt(.99375,48)
B

one_est <- 4150 + (.0007871*230000) + (-13.17*7.50) + (623.6 *0)
two_est <- 4150 + (.0007871*250000) + (-13.17*7.30) + (623.6 *0)
three_est <- 4150 + (.0007871*280000) + (-13.17*7.10) + (623.6 *0)
four_est <- 4150 + (.0007871*340000) + (-13.17*6.90) + (623.6 *0)

new1 <- data.frame(X1=c(230000,250000,280000,340000),X2=c(7.5,7.3,7.1,6.9),X3=c(0,0,0,1))
predict(groc_reg1, new1, se.fit = T, interval = "prediction", leve=0.99375)

one_est - B*147.288
one_est + B*147.288
two_est - B*146.058
two_est + B*146.058
three_est - B*145.134
three_est + B*145.134
four_est - B*145.930
four_est + B*145.930

#QUESTION #6 (7.5; a,b)

#IMPORT DATASET
#NAME vARIABLES
patient <- read.table("~/Math 515/Data Sets/Chapter  6 Data Sets/CH06PR15.txt", quote="\"")
names(patient)[1] <- "Y"
names(patient)[2] <- "X1"
names(patient)[3] <- "X2"
names(patient)[4] <- "X3"

#7.5 Part A
patient_reg.1<-lm(Y~X2, data=patient)
anova(patient_reg.1)

patient_reg.2<-lm(Y~X1+X2, data=patient)
anova(patient_reg.2)

patient_reg.3<-lm(Y~X1+X2+X3, data=patient)
anova(patient_reg.3)

#7.6
patient_reg.4<-lm(Y~X1, data=patient)
anova(patient_reg.4)
anova(patient_reg.3)
qf(.975, 2,42)
1 - pf(4.175,2,42)

#7.9
y <- patient$Y + patient$X1
patient_reg.5<-lm(y~patient$X3)
anova(patient_reg.5)
anova(patient_reg.3)
