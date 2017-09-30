---
title: "Final"
author: "Ziv Schwartz"
date: "5/24/2017"
output: html_document
---
```{r}
library(readr)
library(car)
library(corrplot)
library(MASS)
library(gbm)
library(xgboost)
library(ggplot2)
```

```{r}
load("~/Desktop/final_data.RData")

lafdtraining <- read_csv("~/Downloads/lafdtraining.csv")
training <- lafdtraining
testing.noresponse <- read_csv("~/Downloads/testing.without.response.csv")
testing <- testing.noresponse

colSums(is.na(training))
colSums(is.na(testing))

training$`First in District`[is.na(training$`First in District`)] <- mean(na.omit(training$`First in District`))
training$`Dispatch Sequence`[is.na(training$`Dispatch Sequence`)] <- mean(na.omit(training$`Dispatch Sequence`))
training$`PPE Level`[is.na(training$`PPE Level`)] <- "EMS"
training$`Incident Creation Time (GMT)`[is.na(training$`Incident Creation Time (GMT)`)] <- mean(na.omit(training$`Incident Creation Time (GMT)`))

train <- training
train$`Unit Type` <- as.numeric(as.factor(train$`Unit Type`))
train$`Dispatch Status` <- as.numeric(as.factor(train$`Dispatch Status`))
train$`Dispatch Sequence` <- as.numeric(train$`Dispatch Sequence`)
train$`PPE Level` <- as.numeric(as.factor(train$`PPE Level`))

test <- testing
test$`Unit Type` <- as.numeric(as.factor(test$`Unit Type`))
test$`Dispatch Status` <- as.numeric(as.factor(test$`Dispatch Status`))
test$`Dispatch Sequence` <- as.numeric(test$`Dispatch Sequence`)
test$`PPE Level` <- as.numeric(as.factor(test$`PPE Level`))

set.seed(1995)
boost.train = gbm(`Dispatch Sequence` ~ `year` + `First in District` + `Dispatch Status` + `Unit Type` + `PPE Level`, data=na.omit(test), distribution="gaussian", n.trees=27, interaction.depth=4, shrinkage=0.2)
summary(boost.train)
pred.ds=predict(boost.train, newdata=test[-6], n.trees=27)

table(is.na(test$`Dispatch Sequence`))
test$`Dispatch Sequence`[is.na(test$`Dispatch Sequence`)] <- pred.ds[is.na(test$`Dispatch Sequence`)]
table(is.na(test$`Dispatch Sequence`))

set.seed(1995)
boost.train = gbm(elapsed_time ~ `year` + `First in District` + `Dispatch Sequence` + `Dispatch Status` + `Unit Type` + `PPE Level`, data=na.omit(train), distribution="gaussian", n.trees=27, interaction.depth=4, shrinkage=0.2)
summary(boost.train)

set.seed(1995)
pred.boost=predict(boost.train, newdata=test, n.trees=27)
table(is.na(pred.boost))

pb <- pred.boost
pb[pb > 10000] <- pb[pb > 10000]*1.6
```

#XGBoost
```{r}
str(train)
traintrain <- train[c(3,4,6,7,8,9,11)]

traintrain$year <- as.numeric(traintrain$year)
traintrain$elapsed_time <- as.numeric(traintrain$elapsed_time)
traintrain <- na.omit(traintrain)
et <- traintrain$elapsed_time
traintrain <- traintrain[-7]
traintrain <- as.matrix(traintrain)
str(traintrain)

set.seed(1995)
xgb <- xgboost(data = traintrain, label = et, nthread = 2, nround = 25, eta = 0.1, max_depth = 3, min.child.weight = 8)

names <- dimnames(traintrain)[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix)

str(test)
testtest <- test[c(3,4,6,7,8,9)]

testtest$year <- as.numeric(testtest$year)
testtest$`First in District` <- as.numeric(testtest$`First in District`)
testtest <- as.matrix(testtest)
str(testtest)

set.seed(1995)
preds <- predict(xgb, newdata=testtest)

pb1 <- preds
pb1[pb1 > 10000] <- pb1[pb1 > 10000]*1.6
```

```{r}
predictions <- (pb/2)+(pb1/2)
row.id <- as.integer(testing$row.id)
prediction <- as.integer(predictions)
Final <- cbind(row.id, prediction)
write.csv(Final, file = "/Users/sportsdman/Desktop/Final.csv", row.names = F)
```

```{r}
corrplot(cor(train[,unlist(lapply(train, is.numeric))], use="pairwise.complete.obs"))
cor(train[,unlist(lapply(train, is.numeric))], use="pairwise.complete.obs")
round(cor(train[,unlist(lapply(train, is.numeric))], use="pairwise.complete.obs"),3)

plot(boost.train, i.var = "year")
plot(boost.train, i.var = "`First in District`")
plot(boost.train, i.var = "`Dispatch Sequence`")
plot(boost.train, i.var = "`Dispatch Status`")
plot(boost.train, i.var = "`Unit Type`")
plot(boost.train, i.var = "`PPE Level`")

modnames <- c("1. linear model", "2. lasso-reduced", "3. gbm 100 trees", "4. xgboost", "5. gbm replaced N/A's", "6. gbm with scaled preds", "7. gbm with xgboost")
msevals <- c(1541221, 1554204, 1507624, 1435502, 1411855, 1373334, 1367555.05046)
(nameval <- as.data.frame(cbind(modnames, msevals)))
ggplot(nameval, aes(x=modnames,y=msevals)) + geom_point() + theme_bw()
ggplot(nameval, aes(x=modnames, y=msevals, fill = modnames)) + stat_summary(fun.y="mean", geom="bar") + theme_bw()

hist(log(train$elapsed_time))
hist(log(predictions))
hist(log(pred.boost))
hist(log(pb))
hist(log(pb1))
```



