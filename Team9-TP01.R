#Question 7
#On the USArrests data, show that this proportionality holds.
library(ISLR)
set.seed(1)
#center and scale the columns 
dsc <- scale(USArrests)
#squared euclidean distance
d1<- dist(dsc)^2
### correlation based distance
d2<- as.dist(1-cor(t(dsc)))
summary(d2/d1)
plot(d2/d1)


#Question 8
#Using the sdev output of the prcomp() function

pr.out<- prcomp(USArrests, scale=TRUE)
pr.var<- pr.out$sdev^2
pve<- pr.var/sum(pr.var)
sum(pr.var)
pve
#use the prcomp()function to compute the principal component loadings. 
loadings <- pr.out$rotation
USArrests2 <- scale(USArrests)
sumvar <- sum(apply(as.matrix(USArrests2)^2, 2, sum))
apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum)/sumvar

