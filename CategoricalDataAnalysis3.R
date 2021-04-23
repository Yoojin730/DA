#2번문제
loan = matrix(c(75,10,45, 60,30,80), ncol=2, byrow=TRUE)
scores = c(0,1,2)
scores_2 = c(0,1,4)
loan

fit = glm(loan~scores + scores_2, family=binomial(link=logit))
summary(fit)

fit = glm(loan~scores, family=binomial(link=logit))
summary(fit)

fit = glm(loan~1, family=binomial(link=logit))
summary(fit)

#3번문제
IQ = c(1,1,0,0)
rank=c(1,0,1,0)
sat_h = c(60,54,29,31)
sat_l=c(47,87,78,262)
response=matrix(c(sat_h ,sat_l), ncol=2)
response

fit = glm(response~ rank + IQ + rank:IQ, family=binomial(link=logit))
summary(fit)

fit = glm(response~rank + IQ, family=binomial(link=logit))
summary(fit)


#2-(4)번
log(((75/85)^75))+log(((10/85)^10))+log(((45/105)^45))+log(((60/105)^60))+log(((30/110)^30))+log(((80/110)^80))