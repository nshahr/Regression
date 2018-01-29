plot(PA_Industrial$T, PA_Industrial$C)
plot(PA_Industrial$P, PA_Industrial$C)
plot(PA_Industrial$T, PA_Industrial$P)
r = lm(PA_Industrial$C~PA_Industrial$T+PA_Industrial$P)
summary(r)
summary(PA_Industrial$C)


install.packages("ggplot2")

install.packages("reshape2")

install.packages("RColorBrewer")


library("ggplot2")
library("reshape2")
library("RColorBrewer")

f = data.frame(x = 1 : 10 , y = 1 : 10, z = 2 : 11)
f2 = melt(f, id="x")
f
f2
ggplot(f2, aes(x,value, colour = variable, xlab= "Months"))+ scale_color_manual(values = c("red","black")) + geom_line(size = 1) + geom_point(shape = 16, size = 2)
#+ scale_color_manual(values = c("red","black")) 
head(f2)



x = 10:100
which(x>60)
x[which(x>60)] = 0
x


