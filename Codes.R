 # CODES

##   PAEP=imported data

m2=PAEP$month[13:132]
t2=PAEP$T[13:132]
p2=PAEP$P[13:132]
c2=PAEP$C[13:132]
c3=PAEP$C[1:120]  ##Using last year consumption in the same month
ep=data.frame(m2,t2,p2,c2,c3) ##creating data frame



r=lm(c2~p2+c3+t2+I(p2^2)+I(t2^2),data = ep)  ## regresion using whole data
summary(r)


r2=lm(c2~p2+c3+I(p2^2)+t2+I(t2^2)+I(sin(t2)),data = ep[1:108,]) ##some of data are remained as test data 


## testing model :predicting dependent variable using test data
cc=predict.lm(r,newdata=ep[109:120,]) 
er=100*(cc-c2)/c2
er
summary(er)


plot(c2,type = "l",col="red",lty=1,lwd=3,cex=1,pch=20)
lines(cc,col="blue",lwd=3,pch=10)



## Packages nedded :
install.packages("leaps")
install.packages("bootstrap")
install.packages("rgl")
install.packages("scatterplot3d")

## Loading packages :
library("leaps")
library("bootstrap")
library("rgl")
library("scatterplot3d")
library("MASS")
library("car")



##Model selection :
#1 :
stepAIC(r,direction = "backward")

#2:
anova(r,r2)

#3:
r3=regsubsets(c2~p2+c3+I(p2^3)+t2+I(p2^2)+I(t2^2) ,ep)
plot(r3,scale="adjr2")



##  3D plots :
scatterplot3d(t2,p2,c2,type = "h",pch = 16,highlight.3d = T)

plot3d(t2,p2,c2,col = "red",size = 5)


