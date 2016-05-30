```{r message=F,warning=F}
install.packages("mlbench")
install.packages("caret")
library(mlbench)
data(PimaIndiansDiabetes) 
head(PimaIndiansDiabetes) 
```
```{r message=F,warning=F}
PimaIndiansDiabetesC<- 
  PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),] 
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))
```
```{r message=F,warning=F}
PimaIndiansDiabetesC$Test<-F 

PimaIndiansDiabetesC[
  sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),
  ]$Test<-T 
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T)) 
```
```{r message=F,warning=F}
fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```
```{r message=F,warning=F}
PosPred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
PosAns<-ifelse(PosPred<0.5,"Pos","Neg")
PosAns<-factor(PosAns,levels = c("Pos","Neg"))
library(caret)
```
```{r message=F,warning=F}
sensitivity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
specificity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
posPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
negPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```
