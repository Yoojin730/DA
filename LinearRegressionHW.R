###Linear Regression HW
###Problem 6

data <- read.table("C:\\Users\\shong\\Downloads\\Commercial.txt", header = TRUE)
data


model1 <- lm(formula = data$Y ~ data$x1)
deviance(model1) #SSE


model123 <- lm(formula = data$Y ~ data$x1 + data$x2 + data$x3)
deviance(model123) #SSE

221.739-140.5556

model12 <- lm(formula = data$Y ~ data$x1 + data$x2)
deviance(model12) #SSE

model1234 <- lm(formula = data$Y ~ data$x1 + data$x2 + data$x3 + data$x4)
deviance(model1234)

model124 <- lm(formula = data$Y ~ data$x1 + data$x2+data$x4)
deviance(model124) #SSE

summary(model1234)
summary(model124)

model123 <- lm(formula = data$Y ~ data$x1 + data$x2+ data$x3)
deviance(model123)

summary(model1234)
anova(model1234)

summary(models124)
anova(model124)

anova(model123)



model12 <- lm(formula = data$Y ~ data$x1 + data$x2)
deviance(model12)


qf(p=0.975, df1 = 1, df2 = 42, lower.tail=TRUE)


qf(p=0.95, df1 = 1, df2 = 77, lower.tail=TRUE)





















