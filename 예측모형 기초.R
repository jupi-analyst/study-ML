#예측모형 특성 내표본오차를 최소화 과대적합(x), 일반화.강건학 모형(o). 즉, 외표본 오차를 최소화하는 예측모형이 찾고하나는 모형

## 내표본 오차
library(tidyverse)
library(ggplot2)
library(mlbench)
### 보스턴 집값데이터
data(BostonHousing)
glimpse(BostonHousing)

### 1. 독립변수 선택
ind <- dput(names(BostonHousing))
ind_sel <- setdiff(ind, c('medv', 'chas'))

### 2. 모형 개발
housing_model <- as.formula(paste('medv', '~', paste(ind_sel, collapse = '+'), collapse = ''))
housing_model

### 3. 모형 적합
model <- lm(housing_model, data = BostonHousing[1:100,])
model

### 4. 내표본 예측값 산출
predicted <- predict(model, BostonHousing[1:100, ], type = "response")
predicted

### 5. RMSE 오차 계산
actual <- BostonHousing[1:100, 'medv']
sqrt(mean(predicted - actual)^2)
