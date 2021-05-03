###Alligat,r Example

size = c(1.24,1.30,1.30,1.32,1.32,1.40,1.42,1.42,1.45,1.45,1.47,1.47,1.50,1.52,1.55,1.60,1.63, 1.65, 1.65, 1.65, 1.65, 1.68, 1.70, 1.73, 1.78,1.78, 
         1.78, 1.80, 1.80, 1.85, 1.88, 1.93, 1.98, 2.03, 2.03, 2.16,2.26 , 2.31 , 2.31 , 2.36,2.36 , 2.39 , 2.41, 2.44, 2.46 , 2.56, 2.67 , 2.72, 2.79,
         2.84 , 3.25, 3.28, 3.33, 3.56, 3.58, 3.66, 3.68 ,3.71 ,3.89)

food = c("I","I","I","F","F","F","I","F","I","O","I","F","I","I","I","I","I","O","I","F","F","F","I","O","I","I","O","I","F","F","I","I","I","F","F","F","F","F","F","F","F","F","F","F","F","O", "F","I","F","F","O","O","F","F","F","F","O","F","F")

lst <- list(x=size, y=food)
dat = data.frame(lst)
dat
colnames(dat)<-c("size","food") 
dat

#install.packages("VGAM")
library(VGAM)
fit1 <- vglm(food ~ size, multinomial, data=dat)
summary(fit1)

###Belief Example
belief <- matrix(0,4,5)
belief[,1] <- c(1,1,0,0)
belief[,2] <- c(1,0,1,0)
belief[,3] <- c(371,250,64,25)
belief[,4] <- c(49,45,9,5)
belief[,5] <- c(74,71,15,13)
belief <- data.frame(belief)
names(belief) <- c("Race", "Gender", "Yes", "Un", "No")
belief

#Multinomial logit model
fit1 <- vglm(cbind(Yes, Un, No) ~ Race + Gender + Race:Gender, multinomial, data=belief)
summary(fit1)

fit2 <- vglm(cbind(Yes, Un, No) ~ Race + Gender, multinomial, data=belief)
summary(fit2)

