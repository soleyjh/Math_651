#JAMES HYER SOLEY
#HOMEWORK #1 (R CODE) W/ COMMENTS
#REGRESSION (STAT 515)
#05/27/14

#QUESTION #1 (1.20)
`Q1.20_DATA` <- read.table("~/Math 515/Data Sets/Chapter  1 Data Sets/CH01PR20.txt", quote="\"")

#CLEAN DATA
Q1.20_DATA$X <- Q1.20_DATA$V2
Q1.20_DATA$Y <- Q1.20_DATA$V1
Q1.20_DATA$V1 <- NULL
Q1.20_DATA$V2 <- NULL

#LOOK AT DATA VIA SCATTERPLOT
plot(Q1.20_DATA$Y~Q1.20_DATA$X)

#1.20 PART A
Q1_SLR <- lm(Q1.20_DATA$Y~Q1.20_DATA$X)
Q1_SLR
summary(Q1_SLR)

#1.20 PART B
plot(Q1.20_DATA$Y~Q1.20_DATA$X)
abline(Q1_SLR$coef)

#1.20 PART C
Q1_SLR

#1.20 PART D
Q1_SLR

#1.24 PART B
anova(Q1_SLR)
sqrt(79)

#QUESTION 2.5 (a,b,c,d)
#2.5 Part A
Q1_SLR
confint(Q1_SLR, level=0.90)

#2.5 Part B
summary(Q1_SLR)
abs(qt(.05,43))


#2.5 Part D
abs(qt(.05,43))


#QUESTION #2.14 (a,b)

#2.14 Part A

#Calculate the Standard Error
Q1.20_DATA$SS <- (Q1.20_DATA$X-mean(Q1.20_DATA$X))^2
sum(Q1.20_DATA$SS)
mean(Q1.20_DATA$X)
8.914^2

sqrt(79.4594*((1/45)+((6-5.11111)/340.4444)))
1.404717 * abs(qt(.05,43))

#Confidence Interval Calculation
89.631+2.361429
89.631-2.361429

#2.14 Part B

#Calculate the Standard Error
sqrt(8.914^2+8.914)
9.400713 * abs(qt(.05,43))

#Confidence Interval Calculation
89.631+15.80326
89.631-15.80326

#Question 2.24

#2.24 Part B,c,D
anova(Q1_SLR)
qf(.9,1,43)


