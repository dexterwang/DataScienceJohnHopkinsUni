#1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

library(rpart)
library(ggplot2)
library(rattle)

set.seed(125)

data_train <- segmentationOriginal[segmentationOriginal$Case=="Train",]
data_test <- segmentationOriginal[segmentationOriginal$Case=="Test",]


modFit <- train(Class ~ .,method="rpart",data=data_train)

plot(modFit$finalModel, uniform=TRUE, 
      main="Classification Tree")

text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

fancyRpartPlot(modFit$finalModel)






#2



#3

library(pgmm)
data(olive)
olive = olive[,-1]

model <- train(Area ~.,data=olive,method="rpart")

newdata = as.data.frame(t(colMeans(olive)))

predict(model, newdata)


#4

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

str(trainSA)

model <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")


missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd,predict(model,trainSA))

#missClass(trainSA$chd,predict(model,testSA))


#5 

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)

vowel.test$y <- as.factor(vowel.test$y)

set.seed(33833)

modFit <- train(y~ .,data=vowel.train,method="rf",prox=TRUE)
modFit

varImp(modFit)


