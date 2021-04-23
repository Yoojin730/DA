#흡연/폐암
smoking = matrix(c(10,50,50,100,500,600,450,450,300,150,50,10), ncol=2, byrow=TRUE)
dimnames(smoking) = list(smoke=c(0,2,10,20,40,60),disease=c("cancer", "healthy"))
scores = c(0,2,10,20,40,60)
smoking

res = glm(smoking~scores, family = binomial())
summary(res)

res = glm(smoking~1, family = binomial())
summary(res)

#흡연/폐암(2016)
smoking = matrix(c(7,61,55,129,489,500,475,431,293,154,38,12), ncol=2, byrow=TRUE)
dimnames(smoking) = list(smoke=c(0,2,10,20,40,60),disease=c("cancer", "healthy"))
smoking
scores = c(0,2,10,20,40,60)
smoking

res = glm(smoking~scores, family = binomial())
summary(res)

res = glm(smoking~1, family = binomial(link="identity"))
summary(res)

#city, smoking, cancer
city = c(1,1,0,0)
smoking=c(1,0,1,0)
cancer = c(126, 35, 908, 497)
n=c(226,96,1596,1304)
response=matrix(c(cancer, n-cancer), ncol=2)

fit = glm(response~smoking, family=binomial(link=logit))
summary(fit)

fit = glm(response~city + smoking, family=binomial(link=logit))
summary(fit)

fit = glm(response ~ age + smoking + age:smoking, family=binomial(link=logit))
summary(fit)

#총기류/사형제
mat = matrix(c(784, 236, 311, 66), ncol=2, byrow=TRUE)
mat
dimnames(mat) = list(gun=c(1,0), death=c(1,0))
mat
gun = c(1,0)
death = c(1,0)

res = glm(mat~gun, family = binomial())
summary(res)
res = glm(mat~1, family = binomial())
summary(res)

#age,smoking,breathing
age = c(1,1,0,0)
smoking=c(1,0,1,0)
breathing = c(74,4,57,34)
n=c(319,168,739,611)
response=matrix(c(breathing, n-breathing), ncol=2)

fit = glm(response~smoking, family=binomial(link=logit))
summary(fit)

fit = glm(response~age + smoking, family=binomial(link=logit))
summary(fit)

fit = glm(response ~ age + smoking + age:smoking, family=binomial(link=logit))
summary(fit)


#IQ,rank,sat
IQ = c(1,1,0,0)
rank=c(1,0,1,0)
sat = c(117,54,29,31)
n=c(164, 141, 107, 293)
response=matrix(c(sat , n-sat), ncol=2)
response

fit = glm(response~rank, family=binomial(link=logit))
summary(fit)

fit = glm(response~IQ + rank, family=binomial(link=logit))
summary(fit)

#사회적지위 / 직업만족도
mat = matrix(c(146, 125, 85, 349), ncol=2, byrow=TRUE)
mat
dimnames(mat) = list(rank=c(1,0), sat=c(1,0))
rank = c(1,0)
sat = c(1,0)
mat

res = glm(mat~rank, family = binomial(link=logit))
summary(res)

log(dbinom(10,60,10/60)**dbinom(50,60,50/60))

log(((7/68)^7))+log(((61/68)^61))+log(((55/184)^55))+
log(((129/184)^129))+log(((489/989)^489))+log(((500/989)^500))+
log(((475/906)^475))+log(((431/906)^431))+log(((293/447)^293))+
log(((154/447)^154))+
log(((38/50)^38))+
log(((12/50)^12))

log(((1/6)^60))+log(((5/6)^60))+log(((1/3)^200))+log(((2/3)^400))+log(((5/11)^500))+log(((6/11)^600))+log(((1/2)^900))






2720*log(exp(1/2)/(1+exp(1/2)))





