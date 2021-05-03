###Backward
crabs = read.table("https://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header =T)
head(crabs)
res=glm(y~width+factor(color)+factor(spine), family=binomial, data=crabs)
step(res)

###ROC Curve
x1 = c(80,50,43,121,25,54,76)
x2 = c(28,87,145,110,55,84,85,67)
ylab=factor(c(0,0,0,0,0,0,0,1,1,1,1,1,1,1,1))
#install.packages("pROC")
library(pROC)
res = roc(ylab, c(x1,x2))
res
plot(res)
auc(res)

###
yes = c(32,21,6,3,12,34,3,4,52,5,8,6,35,30,9,11,6,15,17,4,9,21,26,25,21,7,25,31,3,9,
        10,25,25,39,2,4,3,0,29,6,16,7,23,36,4,10)
no = c(81,41,0,8,43,110,1,0,149,10,7,12,100,112,1,11,3,6,0,1,9,19,7,16,10,8,18,37,0,
       6,11,53,34,49,123,41,3,2,13,3,33,17,9,14,62,54)
adm = cbind(expand.grid(gender=c("female","male"), dept=c("anth","astr","chem","clas","comm","comp","engl","geog","geol","germ","hist","lati","ling","math","phil","phys","poli","psyc","reli","roma","soci","stat","zool")),yes, no)

adm$gender=relevel(adm$gender, ref = "male")
res=glm(cbind(yes,no)~dept, family = binomial, data=adm)
sum(resid(res, type="pearson")^2)

resid(res, type="pearson")/sqrt(1-lm.influence(res)$hat)

res=glm(cbind(yes, no)~dept+gender, family=binomial, data=adm)
res$coefficients

res=glm(cbind(yes, no)~gender, family=binomial, data=adm)
res$coefficients


