plot(PA$T, PA$C)
plot(PA$P, PA$C)
plot(PA$T, PA$P)
r = lm(PA$C~PA$T+PA$P)
summary(r)
summary(PA$C)
C1=-924.58*PA$T+238.49*PA$P+61125.93
plot(C1,PA$C)
plot(PA$month, PA$C)
smoothscatter(PA$month, PA$C)
lines(PA$month, PA$C)
lines(PA$month, C1, col="green")
abline(0,1)
r = lm(PA$C~PA$T+I((PA$P)^2))
summary(r)
C2=-934.830*PA$T+9.475*PA$P+62935.829 +6338
plot(C2,PA$C)
lines(PA$month, PA$C)
lines(PA$month, C2, col="green")
plot(PA$month, PA$C)
line(PA$month, PA$C)
lines(PA$month,PA$C)
lines(PA$month, C2, col="red")
lines(PA$month, PA$C)
plot(PA$month,PA$C)
lines(PA$month, PA$C)
lines(PA$month, C2, col="red")
summary(C2)
P = PA$P[1:119]
C = PA$C[1:119]
T = PA$T[1:119]
r2 = lm(C~0+T+P)
summary(r2)
c3=-931.22*PA$T[120]+274.71*PA$P[120]+60732.35
C3
c3
PA$C[120]
c3

summary(hub)
str(hub)
cor(hub[1:118, 7],hub[1:118,13])
cor(hub[1:118, 7],hub[1:118,9])
x=-10:10
y=x^2
cor(x,y)
plot(x,y)
a=hub$`Wtd avg price $/MMBtu`
b=hub$X__2
plot(a[1:118],b[1:118])
cor(a[53:111],b[53:111])
cor(a[1:53],b[1:53])
cor(hub[1:118, 7], hub[1:118, 11])
summary(b)
summary(a[1:118])



#Here we are making regression models based on the last month price (LMP)


CP=PA_R$`Avg Pr`[2:132]
LMP=PA_R$`Avg Pr`[1:131]
T=PA_R$`Avg Temp`[2:132]
C=PA_R$`Avg Con` [2:132]


R = lm (C~CP + T)
summary (R)


R2 = lm (C~LMP + T)
summary (R2)


R3 = lm (`Avg Con`[2:120] ~ `Avg Temp`[2:120] + `Avg Pr`[1:119], data = PA_R)
summary (R3)








#Here is the main

TPA_R = data.frame(Avg.Con = PA_Res$Avg_Con[2:132], Avg.Temp = PA_Res$Avg_Temp[2:132], Avg.Pr = PA_Res$Avg_Pr[1:131])

R3 = lm (Avg.Con ~ Avg.Temp + Avg.Pr, data = TPA_R[1:131, ])
summary(R3)

#to test the model (r test)

RT = lm (Avg.Con ~ Avg.Temp + Avg.Pr, data = TPA_R[1:119, ])

PC=predict.lm (RT, newdata = TPA_R[120:131,])


plot(PA_Res$Avg_Con[121:132], PC)
abline(0,1)


plot(PA_Res$Avg_Con[121:132], type = "l", lty=1)
lines(PC, col='red')
Er=100*(PA_Res$Avg_Con[121:132] - PC)/PA_Res$Avg_Con[121:132]
summary(Er)
plot(Er)

#fitted consumption
RT = lm (Avg.Con ~ Avg.Temp + Avg.Pr, data = TPA_R[1:119, ])
FC=predict.lm (RT, newdata = TPA_R[1:119,])
plot(FC [1:119], type ='l' , col='red')
lines(PA_Res$Avg_Con[2:119])
#we need a new data frame to be able to predict (use predict function)

install.packages("ggplot2")
library("ggplot2")
install.packages("reshape2")
library("reshape2")
install.packages("RColorBrewer")
library("RColorBrewer")


ggplot(TPA_R, aes(Avg.Con, Avg.Temp, col ='red', size=1)) + geom_line(size=1) + geom_point(shape = 16, size = 3)

GGDFrame=data.frame(FC[1:119], PA_Res$Avg_Con[1:119], x=1:119)
MGGDFrame=melt(GGDFrame, id="x")
MGGDFrame2=data.frame(MGGDFrame,y=c(rep(1,119),rep(2,119)))

ggplot(MGGDFrame2, aes(x,value, colour = y)) + geom_line()






