#Commercial - Pennsylvania - same Month
PSD <- as.data.frame(PenStateData)
R1 <- lm(PSD$CC ~ PSD$CP + PSD$T)
R2 <- lm(PSD$CC ~ PSD$CP + PSD$Tabove65)
R3 <- lm(PSD$CC ~ I((PSD$CP)^2) + PSD$T)
R4 <- lm(PSD$CC ~ I((PSD$CP)^2) + PSD$Tabove65)
R5 <- lm(PSD$CC ~ PSD$CPAVG + PSD$T)
R6 <- lm(PSD$CC ~ PSD$CPAVG + PSD$Tabove65)
R7 <- lm(PSD$CC ~ I((PSD$CPAVG)^2) + PSD$T)
R8 <- lm(PSD$CC ~ I((PSD$CPAVG)^2) + PSD$Tabove65)
anova(R1, R2, R3, R4, R5, R6, R7, R8, test = "F")
summary(R1)
summary(R2)
summary(R3)
summary(R4)
summary(R5)
summary(R6)
summary(R7)
summary(R8)

#Commercial - Pennsylvania - last Month
PSD2 <- data.frame(CC = PSD$CC[2:132] , CP = PSD$CP[1:131], T = PSD$T[2:132], CPAVG = PSD$CPAVG[1:131] , Tabove65 = PSD$Tabove65[2:132])
R9 <- lm(CC ~ CP + T, data = PSD2)
R10 <- lm(CC ~ CP + Tabove65, data = PSD2)
R11 <- lm(CC ~ I((CP)^2) + T, data = PSD2)
R12 <- lm(CC ~ I((CP)^2) + Tabove65, data = PSD2)
R13 <- lm(CC ~ CPAVG + T, data = PSD2)
R14 <- lm(CC ~ CPAVG + Tabove65, data = PSD2)
R15 <- lm(CC ~ I((CPAVG)^2) + T, data = PSD2)
R16 <- lm(CC ~ I((CPAVG)^2) + Tabove65, data = PSD2)
anova(R9, R10, R11, R12, R13, R14, R15, R16, test = "F")
summary(R9)
summary(R10)
summary(R11)
summary(R12)
summary(R13)
summary(R14)
summary(R15)
summary(R16)

R17 <- lm(CC ~ I((CP)^2) + CP + T, data = PSD2)
summary(R17)



# Utility - Pennsylvania - last Month
PSD <- as.data.frame(PenStateUtilityData)
PSD2 <- data.frame(CC = PSD$CC[2:132] , CP = PSD$CP[1:131], T = PSD$T[2:132], CPAVG = PSD$CPAVG[1:131] , Tabove65 = PSD$Tabove65[2:132])
R18 <- lm(CC ~ CP + T, data = PSD2)
R19 <- lm(CC ~ CP + Tabove65, data = PSD2)
R20 <- lm(CC ~ I((CP)^2) + T, data = PSD2)
R21 <- lm(CC ~ I((CP)^2) + Tabove65, data = PSD2)
R22 <- lm(CC ~ CPAVG + T, data = PSD2)
R23 <- lm(CC ~ CPAVG + Tabove65, data = PSD2)
R24 <- lm(CC ~ I((CPAVG)^2) + T, data = PSD2)
R25 <- lm(CC ~ I((CPAVG)^2) + Tabove65, data = PSD2)
anova(R18, R19, R20, R21, R22, R23, R24, R25, test = "F")
summary(R18)
summary(R19)
summary(R20)
summary(R21)
summary(R22)
summary(R23)
summary(R24)
summary(R25)

R26 <- lm(CC ~ I((CP)^2) + CP + T, data = PSD2)
summary(R26)


#Industrial - Pennsylvania - last Month
PSD <- as.data.frame(PenStateIndustrialData)
PSD2 <- data.frame(CC = PSD$CC[2:132] , CP = PSD$CP[1:131], T = PSD$T[2:132], CPAVG = PSD$CPAVG[1:131] , Tabove65 = PSD$Tabove65[2:132])
R27 <- lm(CC ~ CP + T, data = PSD2)
R28 <- lm(CC ~ CP + Tabove65, data = PSD2)
R29 <- lm(CC ~ I((CP)^2) + T, data = PSD2)
R30 <- lm(CC ~ I((CP)^2) + Tabove65, data = PSD2)
R31 <- lm(CC ~ CPAVG + T, data = PSD2)
R32 <- lm(CC ~ CPAVG + Tabove65, data = PSD2)
R33 <- lm(CC ~ I((CPAVG)^2) + T, data = PSD2)
R34 <- lm(CC ~ I((CPAVG)^2) + Tabove65, data = PSD2)
anova(R27, R28, R29, R30, R31, R32, R33, R34, test = "F")
summary(R27)
summary(R28)
summary(R29)
summary(R30)
summary(R31)
summary(R32)
summary(R33)
summary(R34)

R35 <- lm(CC ~ I((CP)^2) + CP + T, data = PSD2)
summary(R35)

#Utility - New York - same MOnth
PSD <- as.data.frame(NewYorkUtilityData)
PSD2 <- data.frame(C = PSD$UC[1:132] , P = PSD$UP[1:132], T = PSD$T[1:132], PAVG = PSD$UPAVG[1:132] , Tbelow65 = PSD$Tbelow65[1:132])
R36 <- lm(C ~ P + T, data = PSD2)
R37 <- lm(C ~ P + Tbelow65, data = PSD2)
R38 <- lm(C ~ I((P)^2) + T, data = PSD2)
R39 <- lm(C ~ I((P)^2) + Tbelow65, data = PSD2)
R40 <- lm(C ~ PAVG + T, data = PSD2)
R41 <- lm(C ~ PAVG + Tbelow65, data = PSD2)
R42 <- lm(C ~ I((PAVG)^2) + T, data = PSD2)
R43 <- lm(C ~ I((PAVG)^2) + Tbelow65, data = PSD2)
anova(R36, R37, R38, R39, R40, R41, R42, R43, test = "F")
summary(R36)
summary(R37)
summary(R38)
summary(R39)
summary(R40)
summary(R41)
summary(R42)
summary(R43)

R44 <- lm(C ~ I((P)^2) + P + T, data = PSD2)
summary(R44)




#Utility - Texas - same MOnth
PSD <- as.data.frame(TexasUtilityData)
PSD2 <- data.frame(C = PSD$UC[1:132] , P = PSD$UP[1:132], T = PSD$T[1:132], PAVG = PSD$UPAVG[1:132] , Tbelow65 = PSD$Tbelow65[1:132])
R45 <- lm(C ~ P + T, data = PSD2)
R46 <- lm(C ~ P + Tbelow65, data = PSD2)
R47 <- lm(C ~ I((P)^2) + T, data = PSD2)
R48 <- lm(C ~ I((P)^2) + Tbelow65, data = PSD2)
R49 <- lm(C ~ PAVG + T, data = PSD2)
R50 <- lm(C ~ PAVG + Tbelow65, data = PSD2)
R51 <- lm(C ~ I((PAVG)^2) + T, data = PSD2)
R52 <- lm(C ~ I((PAVG)^2) + Tbelow65, data = PSD2)
anova(R45, R46, R47, R48, R49, R50, R51, R52, test = "F")
summary(R45)
summary(R46)
summary(R47)
summary(R48)
summary(R49)
summary(R50)
summary(R51)
summary(R52)

R53 <- lm(C ~ I((P)^2) + P + T, data = PSD2)
summary(R53)

# Commercial - New York - last Month
PSD <- as.data.frame(NewYorkCommercialData)
PSD2 <- data.frame(CC = PSD$CC[2:132] , CP = PSD$CP[1:131], T = PSD$T[2:132], CPAVG = PSD$CPAVG[1:131] , Tbelow65 = PSD$Tbelow65[2:132])
R54 <- lm(CC ~ CP + T, data = PSD2)
R55 <- lm(CC ~ CP + Tbelow65, data = PSD2)
R56 <- lm(CC ~ I((CP)^2) + T, data = PSD2)
R57 <- lm(CC ~ I((CP)^2) + Tbelow65, data = PSD2)
R58 <- lm(CC ~ CPAVG + T, data = PSD2)
R59 <- lm(CC ~ CPAVG + Tbelow65, data = PSD2)
R60 <- lm(CC ~ I((CPAVG)^2) + T, data = PSD2)
R61 <- lm(CC ~ I((CPAVG)^2) + Tbelow65, data = PSD2)
#anova(R, R19, R20, R21, R22, R23, R24, R25, test = "F")
summary(R54)
summary(R55)
summary(R56)
summary(R57)
summary(R58)
summary(R59)
summary(R60)
summary(R61)

R62 <- lm(CC ~ I((CP)^2) + CP + T, data = PSD2)
summary(R62)






# Industrial - New York - last Month
PSD <- as.data.frame(NewYorkIndustrialData)
PSD2 <- data.frame(C = PSD$IC[2:132] , P = PSD$IP[1:131], T = PSD$T[2:132], PAVG = PSD$IPAVG[1:131] , Tbelow65 = PSD$Tbelow65[2:132])
R63 <- lm(C ~ P + T, data = PSD2)
R64 <- lm(C ~ P + Tbelow65, data = PSD2)
R65 <- lm(C ~ I((P)^2) + T, data = PSD2)
R66 <- lm(C ~ I((P)^2) + Tbelow65, data = PSD2)
R67 <- lm(C ~ PAVG + T, data = PSD2)
R68 <- lm(C ~ PAVG + Tbelow65, data = PSD2)
R69 <- lm(C ~ I((PAVG)^2) + T, data = PSD2)
R70 <- lm(C ~ I((PAVG)^2) + Tbelow65, data = PSD2)
#anova(R, R19, R20, R21, R22, R23, R24, R25, test = "F")
summary(R63)
#summary(R64)
summary(R65)
#summary(R66)
summary(R67)
#summary(R68)
summary(R69)
#summary(R70)

R71 <- lm(C ~ I((P)^2) + P + T, data = PSD2)
sum = summary(R71)
sum
out <- capture.output(cat(
                          #"formula,  " , capture.output(sum$call) , ", " ,
                          capture.output(sum$coefficients) , ", " ,
                          "coef 4 , " , capture.output(sum$coefficients[,4]), ", ",
                          "coef 2,2 , " , capture.output(sum$coefficients[2,2]), ", ",
                          "*** " , R71$coefficients[2], ", ",
                          #capture.output(sum$r.squared) , ", " ,
                          #capture.output(sum$adj.r.squared) , ", " ,
                          #capture.output(sum$fstatistic) , ", " ,
                          "p value of f , " , capture.output(lmp(R71)) , "\n"))
cat(out, file= "test_result.txt", sep="\n", append=TRUE)





























































lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  print(f[1])
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

Main <- function(){
  
  comConsumptionDF <- as.data.frame(CC)
  comPriceAvgDF <- as.data.frame(CPAVG)
  
  resConsumptionDf <- as.data.frame(RC)
  resPriceAvgDF <- as.data.frame(RPAVG)
  
  indConsumptionDF <- as.data.frame(IC)
  indPriceDF <- as.data.frame(IP)

  utlConsumptionDF <- as.data.frame(UC)
  utlPriceDF <- as.data.frame(UP)
  
  weatherDF <- as.data.frame(W)
  
  #AllModelsCom = FindRegression(0, comConsumptionDF, comPriceAvgDF, weatherDF, "Commercial_Results.csv")
  #AllModelsRes = FindRegression(1, resConsumptionDf, resPriceAvgDF, weatherDF, "Residential_Results.csv")
  #AllModelsInd = FindRegression(2, indConsumptionDF, indPriceDF, weatherDF, "Industrial_Results.csv")
  AllModelsUtl = FindRegression(3, utlConsumptionDF, utlPriceDF, weatherDF, "Utility_Results.csv")
}

FindRegression <- function(sector, consumption, price, temperature, filename){
  if(sector == 0 || sector == 1){
    out <- capture.output(cat("Sector, State, Intercept, PriceAVG^2, Temperature, Intercept P Value, PriceAVG^2 P Value, Temperature P Value, R Squared, F Statistics\n"))
    cat(out , file = filename, sep="\n", append = TRUE)
  }else if(sector == 2){
    out <- capture.output(cat("Sector, State, Intercept, Price^2, Price, Temperature, Intercept P Value, Price^2 P Value, Price P Value, Temperature P Value, R Squared, F Statistics\n"))
    cat(out , file = filename, sep="\n", append = TRUE)
  }
  RegModelList <- list()
  for(i in 2:ncol(consumption)){
    # 0 : Commercial
    # 1 : Residential
    # 2 : Industrial
    # 3 : Utility
    sectorName = ""
    if(sector == 0) {sectorName = "Commercial"}
    if(sector == 1) {sectorName = "Residential"}
    if(sector == 2) {sectorName = "Industrial"}
    if(sector == 3) {sectorName = "Utility"}
    if(sector == 0 || sector == 1){ 
      MergedDF = data.frame(Sum.Cons = consumption[[i]][2:132] , Sum.Price = price[[i]][1:131], Sum.Temp = temperature[[i]][2:132])
      if(MergedDF[1, 2]== 0) next
      R = lm(Sum.Cons ~ I((Sum.Price)^2) + Sum.Temp, data = MergedDF)
      sum <- summary(R)
      print(colnames(consumption[i]))
      print(sectorName)
      print(sum)
      out <- capture.output(cat(sectorName , ", " , 
               colnames(consumption)[i] ,", " , 
               sum$coefficients[1,1],", " ,
               sum$coefficients[2,1],", " ,
               sum$coefficients[3,1],", " ,
               sum$coefficients[1,4],", " ,
               sum$coefficients[2,4],", " ,
               sum$coefficients[3,4],", " ,
               sum$r.squared,", " ,
               sum$fstatistic[1], "\n"))
      cat(out, file = filename, sep="\n", append=TRUE)
    }else{
      MergedDF = data.frame(Sum.Cons = consumption[[i]][2:132] , Sum.Price = price[[i]][1:131], Sum.Temp = temperature[[i]][2:132])  
      if(MergedDF[1, 2] == 0) next
      R = lm(Sum.Cons ~ I((Sum.Price)^2) + Sum.Price + Sum.Temp, data = MergedDF)
      R$State <- colnames(consumption)[i]
      RegModelList[[i]] <- R
      sum <- summary(R)
      print(colnames(consumption[i]))
      print(sectorName)
      print(sum)
      out <- capture.output(cat(sectorName , ", " ,
                                colnames(consumption)[i] , ", " ,
                                sum$coefficients[1,1], ", " ,
                                sum$coefficients[2,1], ", " ,
                                sum$coefficients[3,1], ", " ,
                                sum$coefficients[4,1], ", " ,
                                sum$coefficients[1,4], ", " ,
                                sum$coefficients[2,4], ", " ,
                                sum$coefficients[3,4], ", " ,
                                sum$coefficients[4,4], ", " ,
                                sum$r.squared, ", " ,
                                sum$fstatistic[1], "\n"))
      cat(out, file = filename, sep = "\n" , append = TRUE)
    }
  }
  
  AllModels <- do.call(rbind, RegModelList)
  return(AllModels)
}

Main()






require(graphics)



## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

print("***")
summary(lm.D90)$coefficients
summary(lm.D90)$residuals
summary(lm.D90)$effects
summary(lm.D90)$rank
summary(lm.D90)$fitted.values
summary(lm.D90)$assign
summary(lm.D90)$qr
summary(lm.D90)$df.residual
summary(lm.D90)$constrasts
summary(lm.D90)$xlevels
summary(lm.D90)$call
summary(lm.D90)$terms
summary(lm.D90)$model
summary(lm.D90)$fstatistic
summary(lm.D90)$pvalue
lmp(lm.D90)
lm.D90

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)

### less simple examples in "See Also" above