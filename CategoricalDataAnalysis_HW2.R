##1
###Belief Example
belief <- matrix(0,4,4)
belief[,1] <- c(1,1,0,0)
belief[,2] <- c(1,0,1,0)
belief[,3] <- c(370,250,64,25)
belief[,4] <- c(74,71,15,13)

belief <- data.frame(belief)
names(belief) <- c("Race", "Gender", "Yes", "No")
belief

#Multinomial logit model
library(VGAM)
fit1 <- vglm(cbind(Yes, No) ~ Race + Gender + Race:Gender, multinomial, data=belief)
summary(fit1)

###Belief Example
belief <- matrix(0,4,4)
belief[,1] <- c(1,1,0,0)
belief[,2] <- c(1,0,1,0)
belief[,3] <- c(49,45,9,5)
belief[,4] <- c(74,71,15,13)

belief <- data.frame(belief)
names(belief) <- c("Race", "Gender", "Un", "No")
belief

fit2 <- vglm(cbind(Un, No) ~ Race + Gender+ Race:Gender, multinomial, data=belief)
summary(fit2)

##2
#Regression Model
VN <- c(91, 42)
N <- c(990, 350)
A <- c(3758, 1335)
P <- c(1157, 443)
VP <- c(451, 211)

y=c(rep(1,sum(VN)),rep(2,sum(N)),rep(3,sum(A)),rep(4,sum(P)),rep(5,sum(VP)))
x=c(rep(c(1,0),VN),rep(c(1,0),N),rep(c(1,0),A),rep(c(1,0),P),rep(c(1,0),VP))
fit1 = lm(y~x)

summary(fit1)

career <- matrix(0,2,7)
career[,1] <- c(1,0)
career[,2] <- c(91, 42)
career[,3] <- c(990, 350)
career[,4] <- c(3758, 1335)
career[,5] <- c(1157, 443)
career[,6] <- c(451, 211)
career <- data.frame(career)
names(career) <- c("career","VN", "N", "A", "P", "VP")
career

#Proportional Odds
library(VGAM)
fit2 <- vglm(cbind(VN, N, A, P, VP) ~ career, family=cumulative(parallel=TRUE), career)
summary(fit2)

#Unproportional
fit3 <- vglm(cbind(VN, N, A, P, VP) ~ career, family=cumulative(parallel=FALSE), career)
summary(fit3)





##3
SEI = matrix(0,8,4)
SEI[,1] <- c(1,1,1,1,0,0,0,0)
SEI[,2] <- c(1,1,0,0,1,1,0,0)
SEI[,3] <- c(1,0,1,0,1,0,1,0)
SEI[,4] <- c(1105, 14, 411111, 483, 4624, 497, 157342, 1008)
SEI <- data.frame(SEI)
names(SEI) <- c("S","E","I","COUNT")
SEI

f2 = glm(COUNT~S+E+I+S:E+S:I, family=poisson(link = 'log'), data=SEI)
summary(f2)







