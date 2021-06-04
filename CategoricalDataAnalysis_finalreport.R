library(pROC)

#install.packages("mlbench")
library(mlbench)

data(PimaIndiansDiabetes)
pima <- PimaIndiansDiabetes

pima<-subset(pima, pressure>0)
pima<-subset(pima, mass>0)
pima<-subset(pima, glucose>0)

pressure=roc(pima$diabetes, pima$pressure, ci=TRUE)
mass=roc(pima$diabetes, pima$mass, ci=TRUE)
glucose=roc(pima$diabetes, pima$glucose, ci=TRUE)


plot(pressure, col = "red", legacy.axes=TRUE)

lines.roc(mass, col = "blue",lty=2)

lines.roc(glucose, col = "black",lty=3)
