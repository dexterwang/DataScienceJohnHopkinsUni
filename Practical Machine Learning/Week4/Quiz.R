#1

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)

vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

md1 <- train(y~.,data=vowel.train,method="rf")

md2 <- train(y~.,data=vowel.train,method="gbm")

pd1 <- predict(md1,newdata=vowel.test)

pd2 <- predict(md2,newdata=vowel.test)


acc_pd1 <- sum(vowel.test$y==pd1)/length(pd1)

acc_pd2 <- sum(vowel.test$y==pd2)/length(pd1)

acc_pd1

acc_pd2

_pd1
[1] 0.6147186
> 
> acc_pd2
[1] 0.5367965



#aggreed accuracy

sum(vowel.test[pd1==pd2,"y"]==pd1[pd1==pd2])/sum(pd1==pd2)

0.6656051


#2


library(caret)

library(gbm)

set.seed(3433)

library(AppliedPredictiveModeling)

data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]

training = adData[ inTrain,]

testing = adData[-inTrain,]

set.seed(62433)

md1 <- train(diagnosis~.,data=training,method="rf")

md2 <- train(diagnosis~.,data=training,method="gbm")

md3 <- train(diagnosis~.,data=training,method="lda")

pd1 <- predict(md1,newdata=testing)

pd2 <- predict(md2,newdata=testing)

pd3 <- predict(md3,newdata=testing)

# accuracies for each 
sum(testing[,"diagnosis"]==pd1)/length(pd1)

sum(testing[,"diagnosis"]==pd2)/length(pd2)

sum(testing[,"diagnosis"]==pd3)/length(pd3)

> sum(testing[,"diagnosis"]==pd1)/length(pd1)
[1] 0.7682927
> 
> sum(testing[,"diagnosis"]==pd2)/length(pd2)
[1] 0.7926829
> 
> sum(testing[,"diagnosis"]==pd3)/length(pd3)
[1] 0.7682927

#stacked model

comb_train <- data.frame(pd1=pd1,pd2=pd2,pd3=pd3,diagnosis=testing[,"diagnosis"])

md4 <- train(diagnosis~.,data=comb_train,method="rf")

pd4<- predict(md4,newdata=comb_train)

sum(testing[,"diagnosis"]==pd4)/length(pd4)

[1] 0.804878


#3


set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(233)

model = train(CompressiveStrength ~ ., method = 'lasso', data = training)
plot.enet(model$finalModel, xvar="penalty", use.color=TRUE)



#4

setwd("C:/D/R/Practical Machine Learning/Week4")

library(lubridate) # For year() function below

library(forecast)

dat = read.csv("./gaData.csv")

training = dat[year(dat$date) < 2012,]

testing = dat[(year(dat$date)) > 2011,]

tstrain = ts(training$visitsTumblr)

model = bats(tstrain)

pd <- forecast.bats(model, h=nrow(testing),level=95)

accuracy(pd, testing$visitsTumblr)

acc = sum(testing$visitsTumblr <= pd$upper & testing$visitsTumblr >= pd$lower) / nrow(testing)

[1] 0.9617021



#5

set.seed(3523)

library(AppliedPredictiveModeling)

data(concrete)

inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]

training = concrete[ inTrain,]

testing = concrete[-inTrain,]

set.seed(325)




