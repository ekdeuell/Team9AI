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


###Question 8#####
#Using the sdev output of the prcomp() function
#perfomring the PCA using prcomp()
pr.out<- prcomp(USArrests, scale=TRUE)
summary(pr.out)
### we can see that the proportion of variance explained by first Priniciple analysis is 62%

#how we access the variance explained by  each principle copnoent analysis is shown through squaring the standard deviations:
pr.var<- pr.out$sdev^2

#to compute the proportion explained by each principle component we simply divide the variance explianed by each principle component analysis
pve<- pr.var/sum(pr.var)
pve
par(mfrow= c(1,2))

#PVE Explained by Each Component
plot(pve,xlab= "Principle Component", 
     ylab= "Proportion of Variance Explained",
     ylim= c(0,1),
     type= 'b')
#Cumulative PVE
plot(cumsum(pve), xlab= "Principle Component",
       ylab= "Cumulative Proportion of Variance Explained",
       ylim = c(0,1), type='b')



#use the prcomp()function to compute the principal component loadings. 
names(pr.out)
loadings <- pr.out$rotation
#The rotation matrix provides the principal component loadings; each column of pr.out$rotation contains the corresponding principal component loading vector.
USArrests2 <- scale(USArrests)
sumvar <- sum(apply(as.matrix(USArrests2)^2, 2, sum))
apply((as.matrix(USArrests2) %*% loadings)^2, 2, sum)/sumvar
