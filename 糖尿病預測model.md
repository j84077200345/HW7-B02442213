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
 'data.frame':	768 obs. of  9 variables:
 $ pregnant: num  6 1 8 1 0 5 3 10 2 8 ...
 $ glucose : num  148 85 183 89 137 116 78 115 197 125 ...
 $ pressure: num  72 66 64 66 40 74 50 0 70 96 ...
 $ triceps : num  35 29 0 23 35 0 32 0 45 0 ...
 $ insulin : num  0 0 0 94 168 0 88 0 543 0 ...
 $ mass    : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 0 ...
 $ pedigree: num  0.627 0.351 0.672 0.167 2.288 ...
 $ age     : num  50 31 32 21 33 30 26 29 53 54 ...
 $ diabetes: Factor w/ 2 levels "neg","pos": 2 1 2 1 2 1 2 1 2 2 ...
```
###選資料完整的row
```{r}
PimaIndiansDiabetesC<-
  PimaIndiansDiabetes[complete.cases(PimaIndiansDiabetes),]
c(nrow(PimaIndiansDiabetes),nrow(PimaIndiansDiabetesC))
```
```{r}
[1] 768 768
```
### 將資料隨機分為訓練組與測試組

隨機將1/3的資料分到訓練組（Test==F），剩下1/3為測試組（Test==T）

```{r}
PimaIndiansDiabetesC$Test<-F 
PimaIndiansDiabetesC[sample(1:nrow(PimaIndiansDiabetesC),nrow(PimaIndiansDiabetesC)/3),]$Test<-T 
c(sum(PimaIndiansDiabetesC$Test==F),sum(PimaIndiansDiabetesC$Test==T)) 
```
[1] 512 256
我們可以得到訓練組案例數為`r sum(PimaIndiansDiabetesC$Test==F`，測試組案例數為`r sum(PimaIndiansDiabetesC$Test==T)`

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
