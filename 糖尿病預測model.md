#糖尿病預測model

## 資料前處理

### 資料讀取

記載受測者經檢查後糖尿病的陰陽性反應椰果，一共有8個參數，代表各種可能是導致其獲得糖尿病的因素。另外，分類結果為二元分類，包括陽性(pos) 與陰性 (neg) 。

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
