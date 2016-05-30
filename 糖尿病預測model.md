#糖尿病預測model

## 資料前處理

### 資料讀取

記載受測者經檢查後糖尿病的陰陽性反應椰果，一共有8個參數，代表各種可能是導致其獲得糖尿病的因素。另外，分類結果為二元分類，包括陽性(pos) 與陰性 (neg) 。

```{r message=F,warning=F}
install.packages("mlbench")
install.packages("caret")
library(mlbench)
data(PimaIndiansDiabetes) 
str(PimaIndiansDiabetes) 
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
```{r message=F,warning=F}
[1] 512 256
```
我們可以得到訓練組案例數為`r sum(PimaIndiansDiabetesC$Test==F`，測試組案例數為`r sum(PimaIndiansDiabetesC$Test==T)`

## 預測模型建立

### 模型建立
   
由於變數多達8項，且多為連續變項，而輸出為二元類別變項，故選擇邏輯迴歸演算法建立模型，並使用雙向逐步選擇最佳參數組合。

```{r warning=F,message=F}
fit<-glm(diabetes~., PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```
```{r warning=F,message=F}
 Estimate   Std. Error    z value     Pr(>|z|)
(Intercept) -8.790367220 0.8689584635 -10.115981 4.692923e-24
pregnant     0.111238301 0.0374095919   2.973524 2.944018e-03
glucose      0.037838437 0.0046249467   8.181378 2.806163e-16
insulin     -0.002504772 0.0009200346  -2.722476 6.479466e-03
mass         0.071177186 0.0163958998   4.341158 1.417340e-05
pedigree     0.768527895 0.3443952802   2.231529 2.564614e-02
age          0.016656436 0.0110652585   1.505291 1.322492e-01
```
### 模型說明

由上述參數可知，人體內的各項因素是否和糖尿病的產生有所關連，以邏輯迴歸建立模型預測糖尿病是否為陽性，經最佳化後，模型使用參數為`r rownames(summary(finalFit)$coefficient)[-1]`，共`r nrow(summary(finalFit)$coefficient)`個參數，各參數代表從某一個人體內的因素影響糖尿病陰陽性的程度
 
## 預測模型驗證

```{r warning=F,message=F,fig.height=4.5}
PosPred<-predict(finalFit,newdata = PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,])
PosAns<-ifelse(PosPred<0.5,"Pos","Neg")
PosAns<-factor(PosAns,levels = c("Pos","Neg"))
library(caret)
sensitivity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
specificity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
posPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
negPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$diabetes)
```

人體內的各種因素對糖尿病陰陽性的影響，以邏輯迴歸模型預測是否造成陽性反應，可得：

- 敏感度 `r sensitivity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%= 
- 特異性 `r specificity(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%=
- 陽性預測率 `r posPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%=
- 陰性預測率 `r negPredValue(PosAns,PimaIndiansDiabetesC[PimaIndiansDiabetesC$Test==T,]$Class)*100`%=
```
