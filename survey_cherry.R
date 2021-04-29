diameter<-c(8.3, 8.6 , 8.8 , 10.5 , 10.7 , 10.8, 11, 11, 11.1, 11.2,11.3,11.4,11.4,11.7,12,12.9,
12.9,13.3,13.7,13.8,14,14.2,14.5,16,16.3,17.3,17.5,17.9,18,18,20.6)

volume <- c(10.3,
            10.3,
            10.2,
            16.4,
            18.8,
            19.7,
            15.6,
            18.2,
            22.6,
            19.9,
            24.2,
            21,
            21.4,
            21.3,
            19.1,
            22.2,
            33.8,
            27.4,
            25.7,
            24.9,
            34.5,
            31.7,
            36.3,
            38.3,
            42.6,
            55.4,
            55.7,
            58.3,
            51.5,
            51,
            77)
mean(volume)
standard_error <- function(x) sd(x) / sqrt(length(x))
standard_error(volume)

sd(volume)

diameter[1]

library(ggplot2)
plot(x=diameter, y=volume)


length(diameter)


sum(diameter)







standard_error(diameter)
sd(diameter)

2*pnorm(-0.9)
