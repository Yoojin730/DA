set.seed(123)

#(a) Generate a population of size 1000. 
pop <- c(1:1000)

#(b) Take a simple random sample (without replacement) of size 10 and obtain a sample mean. 
samp <- sample(pop, 10)
mean(samp)

#(c) Repeat (b) many times (say 100). Then, you will have 100 different sample means. Obtain the sample mean and variance of the 100 sample means. 
mean_sample <- c()
for (i in 1:100){
  mean_sample <- append(mean_sample, mean(sample(pop, 10)))
}

var_sample <- c()
for (i in 1:100){
  var_sample <- append(var_sample, var(sample(pop, 10)))
}

#Also, construct a histogram (or boxplot).
par ( mfrow = c(1,2)) 
hist(mean_sample)
hist(var_sample)
 

#(d) Repeat (b) and (c) for different sample sizes: 50 and 100.
#50
mean_sample <- c()
for (i in 1:100){
  mean_sample <- append(mean_sample, mean(sample(pop, 50)))
}

var_sample <- c()
for (i in 1:100){
  var_sample <- append(var_sample, var(sample(pop, 50)))
}

par ( mfrow = c(1,2)) 
hist(mean_sample)
hist(var_sample)

#100
mean_sample <- c()
for (i in 1:100){
  mean_sample <- append(mean_sample, mean(sample(pop, 100)))
}

var_sample <- c()
for (i in 1:100){
  var_sample <- append(var_sample, var(sample(pop, 100)))
}

par ( mfrow = c(1,2)) 
hist(mean_sample)
hist(var_sample)



# What can you tell about your findings?