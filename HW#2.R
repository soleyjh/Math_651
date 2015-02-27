#JAMES HYER SOLEY
#HOMEWORK #2 (R CODE) W/ COMMENTS
#REGRESSION (STAT 515)
#06/02/14

#QUESTION #1 (3.4;c,d,e,h)
`Q1.20_DATA` <- read.table("~/Math 515/Data Sets/Chapter  1 Data Sets/CH01PR20.txt", quote="\"")

#CLEAN DATA
Q1.20_DATA$X <- Q1.20_DATA$V2
Q1.20_DATA$Y <- Q1.20_DATA$V1
Q1.20_DATA$V1 <- NULL
Q1.20_DATA$V2 <- NULL

#Create Regression Model in R
Q1_SLR <- lm(Y~X, data=Q1.20_DATA)
stem(Q1_SLR$resi)
hist(Q1_SLR$resi)

Q1_SLR$fitted.values
Q1_SLR$resi

plot(Q1_SLR$resi~Q1_SLR$fitted.values)
plot(Q1_SLR$resi~Q1.20_DATA$X)
qqnorm(Q1_SLR$resi)
qqline(Q1_SLR$resi)


Q1.20_DATA$resi <- sort(Q1_SLR$resi)
Q1.20_DATA$k <- as.numeric(row.names(Q1.20_DATA))
Q1.20_DATA$kn <- (Q1.20_DATA$k-.375)/(45+.25)
Q1.20_DATA$kn_z <- qnorm(Q1.20_DATA$kn)
Q1.20_DATA$e_resi <- Q1.20_DATA$kn_z*8.914
cor(Q1.20_DATA$resi,Q1.20_DATA$e_resi)

plot(Q1.20_DATA$resi~Q1.20_DATA$e_resi)

shapiro.test(Q1_SLR$resi)


CH03PR04 <- read.delim("~/Math 515/Data Sets/Chapter  3 Data Sets/CH03PR04.txt", header=F)
CH03PR04$Y <- CH03PR04$V1
CH03PR04$X_1 <- CH03PR04$V2
CH03PR04$X_2 <- CH03PR04$V3
CH03PR04$X_3 <- CH03PR04$V4
plot(Q1_SLR$resi~CH03PR04$X_2)
plot(Q1_SLR$resi~CH03PR04$X_3)

Q1_SLR <- lm(Q1.20_DATA$Y~Q1.20_DATA$X)
Q1_LOF <- lm(Q1.20_DATA$Y~as.factor(Q1.20_DATA$X))
anova(Q1_SLR,Q1_LOF)


#QUESTION #3
CH03PR17 <- read.table("~/Math 515/Data Sets/Chapter  3 Data Sets/CH03PR17.txt", quote="\"")
CH03PR17$Y <- CH03PR17$V1
CH03PR17$X <- CH03PR17$V2
plot(CH03PR17$Y~CH03PR17$X)

sqrt_slr <- lm(sqrt(CH03PR17$Y)~CH03PR17$X)
summary(sqrt_slr)

plot(sqrt(CH03PR17$Y)~CH03PR17$X)
abline(sqrt_slr$coef)

plot(sqrt_slr$resi~sqrt_slr$fitted.values)
qqnorm(sqrt_slr$resi)
qqline(sqrt_slr$resi)

summary(sqrt_slr)

CH03PR17$resi <- sort(sqrt_slr$resi)
CH03PR17$k <- as.numeric(row.names(CH03PR17))
CH03PR17$kn <- (CH03PR17$k-.375)/(10 +.25)
CH03PR17$kn_z <- qnorm(CH03PR17$kn)
CH03PR17$e_resi <- CH03PR17$kn_z*.3622
plot(CH03PR17$resi~CH03PR17$e_resi)


#sTANDARD ERROR (CALCULATION PULLED FROM HW#1)
SE_3 <- sqrt(79.4594*((1/45)+((3-5.11111)^2/340.4444)))
SE_5 <- sqrt(79.4594*((1/45)+((5-5.11111)^2/340.4444)))
SE_7 <- sqrt(79.4594*((1/45)+((7-5.11111)^2/340.4444)))
SE_3
SE_5
SE_7

#BONFERRONI
B <- abs(qt(1-(.1/6),43))
B

#WORKING HOTELINsG
W <- sqrt(2*(qf(1-.1,2,43)))
W

#PREDICTED VALUES
est_3 <- (-.5802) + 15.0352*(3)
est_5 <- (-.5802) + 15.0352*(5)
est_7 <- (-.5802) + 15.0352*(7)
est_3
est_5
est_7

new <- 
new2 <- 
predict(Q1_SLR, data.frame(X=c(3,5,7)), se.fit = T, interval = "confidence", leve=0.983)
predict(Q1_SLR, data.frame(X=c(4,7)), se.fit = F, interval = "prediction", leve=0.983)
Q1_SLR

# 7. predicting Y, CI and PI
predict(pisa.SLR, newdata=data.frame(Year=112), se.fit = T, interval = "confidence", leve=0.95)
predict(pisa.SLR, newdata=data.frame(Year=112), se.fit = F, interval = "prediction", leve=0.95)


#CI WORKING HOTELING
est_3 + W * SE_3
est_3 - W * SE_3
est_5 + W * SE_5
est_5 - W * SE_5
est_7 + W * SE_7
est_7 - W * SE_7

#CI BONFERRONI
est_3 + B * SE_3
est_3 - B * SE_3
est_5 + B * SE_5
est_5 - B * SE_5
est_7 + B * SE_7
est_7 - B * SE_7

#PART B
#BONFERRONI
B <- abs(qt(1-(.1/4),43))
B

#SCHAFFE
S <- sqrt(2*(qf(1-.1,2,43)))
S

#PART C
SE_4 <- 9.02797
SE_7 <- 9.05808
est_4 <- (-.5802) + 15.0352*(4)
est_7 <- (-.5802) + 15.0352*(7)
SE_4
SE_7
est_4
est_7

#CI SCHAFFE
est_4 + S * SE_4
est_4 - S * SE_4
est_7 + S * SE_7
est_7 - S * SE_7

#CI BONFERRONI
est_4 + B * SE_4
est_4 - B * SE_4
est_7 + B * SE_7
est_7 - B * SE_7
