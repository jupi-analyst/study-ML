library(tidyverse)
library(reshape)
library(readr)
data <- read_csv('./german_credit.csv', col_names = T)
head(data)
F <- c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20) # 요인변환
data.F <- data[, F]; 
data.NF <- data[, -F]

data.F <- sapply(data.F, as.factor)
data <- cbind(data.F, data.NF)
str(data)
d <- sort(sample(nrow(data), nrow(data)*.6))
train <- data[d, ] # 60%
test <- data[-d, ] # 40%
# 모형 적합
## 이항 회귀 분석
logit.m <- glm(as.numeric(Creditability)~., data = train, family = binomial('logit'), control = list(maxit = 50))
summary(logit.m)

## 나무모형
library(rpart)
library(rpart.plot)
rpart.fit <- rpart(Creditability~., data = train)
plot(rpart.fit); text(rpart.fit); prp(rpart.fit, type = 1, extra = 1)

## SVM
library(e1071)
svm.fit <- svm(as.numeric(Creditability) ~ ., train, probability = TRUE)
svm.fit
summary(svm.fit)
# 성능 평가
library(ROCR)
test$prob <- predict(logit.m, test, type = 'response')
logit.pred <- prediction(test$prob, test$Creditability)
logit.pred
logit.perf <- performance(logit.pred, 'tpr', 'fpr')
logit.perf
plot(logit.perf)

## KS 통계량(면적)
max(attr(logit.perf, 'y.values')[[1]] - attr(logit.perf, 'x.values')[[1]])

# 신용점수함수에 가장 영향을 주는 변수 3개
logit.pick.3 <- predict(logit.m, type = 'term', test)

ftopk <- function(x, top = 3){
  res <- names(x)[order(x, decreasing = TRUE)][1:top]
  paste(res, collapse = ';', sep='')
}

# 신용카드 발급 신청자 별로 가장 영향력있는 변수 3개
logit.pick.3 <- apply(logit.pick.3, 1, ftopk, top = 3)
# 추출한 3개 변수를 신용카드 발급 신청자 별로 결합
test <- cbind(test, logit.pick.3)

# 나무 모형
test$rpart_class <- predict(rpart.fit, type = 'class', test)
rpart_score <- predict(rpart.fit, type = 'prob', test)
rpart.pred <- prediction(rpart_score[, 2], test$Creditability)
rpart.perf <- performance(rpart.pred, 'tpr', 'fpr')
plot(rpart.perf)

## SVM 미완성
test$svm_class <- predict(svm.fit, type = 'class', test)
svm.score <- predict(svm.fit, type = 'prob', test, probability = TRUE)
svm.pred <- prediction(attr(svm.score, 'probabilities')[,1], test$Creditability)
svm.perf <- performance(svm.pred, 'tpr', 'fpr')

# 종합 알고리즘 성능평가
## ROC

plot(logit.perf, col = 'red', lty = 1, main = '');
plot(rpart.perf, col = 'blue', lty = 3, add = TRUE);
#plot(svml.perf, col = 'green', add = TRUE, lty = 2);
legend(0.6, 0.6, c('logstic', 'rpart'), col = c('blue', 'red'), lwd = 3)

