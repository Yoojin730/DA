library(readxl)
cherry <- read.csv("C:\\Users\\shong\\Desktop\\표본추출\\cherry.csv")
cherry

VolDia = lm(volume~diameter, data = cherry) #Create the linear regression
summary(VolDia)
