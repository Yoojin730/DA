###GLM for Binary Data###
snoring = matrix(c(24,1355,35,603,21,192,30,224), ncol=2, byrow=TRUE)
snoring
dimnames(snoring) = list(snore=c("never","sometimes","often","always"),
                         heartdisease=c("yes","no"))
snoring
scores = c(0,2,4,5)

#linear
glm(snoring~scores, family=binomial(link="identity"))

#logit
glm(snoring~scores, family = binomial())

#probit
glm(snoring~scores, family=binomial(link="probit"))

###GLM for Count Data###
card = c(1,2,2,5,6,7)
income=c(1,1,2,2,3,4)
#linear(LS fit)
glm(card~income, family = gaussian)
#linear(ML fit)
glm(card~income, family=poisson(link="identity"))
#log linear(ML fit)
glm(card~income, family=poisson(link="log"))


###Analysis with Logit Model : Saturated Model###
race=c(1,1,0,0)
azt=c(1,0,1,0)
symptoms = c(14,32,11,12)
n=c(107,113,63,55)
response=matrix(c(symptoms, n-symptoms), ncol=2)

fit = glm(response~race+azt, family=binomial(link=logit))
summary(fit)

fit = glm(response ~ race + azt + race:azt, family=binomial(link=logit))
summary(fit)




crabs = read.table("https://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header =T)
head(crabs)
res = glm(sat~1, family=poisson, data=crabs)
summary(res)
res = glm(sat~width, family=poisson, data=crabs)
summary(res)

res = glm(y~width, family=binomial, data=crabs)
res

#grouped data
attach(crabs)
grp=cut(width, breaks = c(0,23.25, 24.25, 25.25, 26.25, 27.25, 28.25, 29.25, Inf), dig.lab=4)
cases=table(grp)
avgwt=tapply(width, grp, mean)
toty=tapply(y, grp, sum)
crabs.g=data.frame(cbind(width=avgwt, cases=cases, satell=toty))
crabs.g

res=glm(cbind(satell, cases-satell)~width, family=binomial, data=crabs.g)
res

res = glm(y~width, family=binomial, data=crabs)
summary(res)
#width and 4 colors
res = glm(y~width+factor(color), family=binomial, data=crabs)
summary(res)

#width and 2 colors
res = glm(y~width+factor(color, levels=c(1,2,3,4),labels=c(1,1,1,2)), family=binomial, data=crabs)
summary(res)
