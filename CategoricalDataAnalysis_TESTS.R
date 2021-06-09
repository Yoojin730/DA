rm(list=ls())
x <- c(rep(1,150), rep(0,161))
y <- c(rep(1,135),rep(0,15),rep(1,120),rep(0,41))

fit = lm(y~x)
summary(fit)

mat <- matrix(c(135, 15, 120, 41),2,2,byrow=T)
dimnames(mat) = list(c("Med","Placebo"), c("Normal","Abnormal"))
mat

chisq.test(mat, correct=F)

cor(x,y)
cor = cor(x,y)
(311-1)*(cor**2)
qchisq(0.975,1)

belief <- matrix(0,4,4)
belief[,1] <- c(1,1,0,0)
belief[,2] <- c(1,0,1,0)
belief[,3] <- c(117,54,29,31)
belief[,4] <- c(47,41,78,262)
belief <- data.frame(belief)
names(belief) <- c("IQ", "ST", "HIGH", "LOW")
belief

library(VGAM)
fit2 <- vglm(cbind(HIGH, LOW) ~ IQ + ST + IQ:ST, multinomial, data=belief)
summary(fit2)

belief
library(VGAM)
fit1 <- vglm(cbind(HIGH, LOW) ~ IQ + ST, multinomial, data=belief)
summary(fit1)