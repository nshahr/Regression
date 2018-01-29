plot(PA_Com$month, PA_Com$Avg_Con, PA_Com$Avg_Temp, type ="l")
install.packages("scaterplot3d")
install.packages("rgl")
library("rgl")
plot3d(PA_Com$month, PA_Com$Avg_Con, PA_Com$Avg_Temp, size = 5, col = "red")
ggplot(PA_Com , aes( month, Avg_Con, col=Avg_Temp)) + geom_point(shape=16, size = 3) + geom_line(size = 0.8) + scale_color_gradient(low = "blue", high = "red")

ggplot(PA_Com , aes( Avg_Temp, Avg_Con)) + geom_point(shape=16, size = 3) + geom_line(size = 0.8) 
ggplot(PA_Com , aes( Avg_Pr, Avg_Con)) + geom_point(shape=16, size = 3) + geom_line(size = 0.8) 

cor(PA_Com$Avg_Con, PA_Com$Avg_Temp)
cor(PA_Com$Avg_Con,PA_Com$Avg_Pr)
#cor(PA_Com$Avg_Con,((PA_Com$Avg_Pr)-12.5)^2)

R= lm( Avg_Con ~ Avg_Temp + Avg_Pr, data=PA_Com)
#head(PA_Com)
summary(R)


PA_Com2 = data.frame(Avg_Con2 = PA_Com$Avg_Con[2:132], Avg_Temp2 = PA_Com$Avg_Temp[2:132], Avg_Pr2 = PA_Com$Avg_Pr[1:131])

R2 = lm(Avg_Con2 ~ Avg_Temp2 + Avg_Pr2, data=PA_Com2)
summary(R2)

PA_Com3 = data.frame(Avg_Con3 = PA_Com$Avg_Con[3:132], Avg_Temp3 = PA_Com$Avg_Temp[3:132], Avg_Pr3 = PA_Com$Avg_Pr[1:130])
R3 = lm(Avg_Con3 ~ Avg_Temp3 + Avg_Pr3, data=PA_Com3)
summary(R3)
ggplot(PA_Com3 , aes( Avg_Pr3, Avg_Con3)) + geom_point(shape=16, size = 3) + geom_line(size = 0.8) 




###Just to test last 3 month 
PA_Com4 = data.frame(Avg_Con4 = PA_Com$Avg_Con[4:132], Avg_Temp4 = PA_Com$Avg_Temp[4:132], Avg_Pr4 = PA_Com$Avg_Pr[1:129])
R4 = lm(Avg_Con4 ~ Avg_Temp4 + Avg_Pr4, data=PA_Com4)
summary(R4)


###Adding last year consumption to the model in order to calculate the hidden variables (technology change, population, economic factors, ...)
PA_Com5 = data.frame(Avg_Con5 = PA_Com$Avg_Con[13:132], Avg_Temp5 = PA_Com$Avg_Temp[13:132], Avg_LCon5 = PA_Com$Avg_Con[1:120])
head(PA_Com5)
R5 = lm(Avg_Con5 ~ Avg_Temp5 + Avg_LCon5, data=PA_Com5)
summary(R5)



###test models for prediction (the one has lower error percentage is the better)



PPA_Com3 = data.frame(Avg_Con3 = PA_Com$Avg_Con[1:132], Avg_Temp3 = PA_Com$Avg_Temp[1:132])
RP3 = lm(Avg_Con3 ~ Avg_Temp3 , data=PPA_Com3[1:120, ])
summary(RP3)




PPA_Com4 = data.frame(Avg_Con4 = PA_Com$Avg_Con[4:132], Avg_Temp4 = PA_Com$Avg_Temp[4:132], Avg_Pr4 = PA_Com$Avg_Pr[1:129])
RP4 = lm(Avg_Con4 ~ Avg_Temp4 + Avg_Pr4, data=PPA_Com4[1:117, ])
summary(RP4)



PPA_Com5 = data.frame(Avg_Con5 = PA_Com$Avg_Con[13:132], Avg_Temp5 = PA_Com$Avg_Temp[13:132], Avg_LCon5 = PA_Com$Avg_Con[1:120])
RP5 = lm(Avg_Con5 ~ Avg_Temp5 + Avg_LCon5, data=PPA_Com5[1:108, ])
summary(RP5)


#predicting the last year

PC1 = predict.lm(RP3, newdata = PPA_Com3[121:132,])
PC2 = predict.lm(RP4, newdata = PPA_Com4[118:129,])
PC3 = predict.lm(RP5, newdata = PPA_Com5[109:120,])

#calculating error percentage

C = PA_Com$Avg_Con[121:132]

E1 = 100*(C - PC1)/C
E2 = 100 * (C - PC2)/C
E3 = 100 * (C - PC3)/C


plot(E1, type ="l", col="red")
lines(E2, col="green")
lines(E3, col="blue")

plot(C, type="l", col= "black")
lines(PC1, col= "red")
lines(PC2, col= "green")
lines(PC3, col = "blue")
