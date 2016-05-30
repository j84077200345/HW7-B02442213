install.packages("mlbench")
install.packages("caret")
library(mlbench)
data(PimaIndiansDiabetes) 
head(PimaIndiansDiabetes) 

PimaIndiansDiabetesC<- 
  PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),] 
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))

PimaIndiansDiabetesC$Test<-F 
#隨機取1/3當Test set
PimaIndiansDiabetesC[
  sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),
  ]$Test<-T 
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T)) 

fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients

MinePred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
MineAns<-ifelse(MinePred<0.5,"M","R")
MineAns<-factor(MineAns,levels = c("M","R"))
library(caret)
sensitivity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
specificity(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
posPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
negPredValue(MineAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
