#2 


library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]


ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=Cement),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(Cement)),alpha=0.5)


ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(BlastFurnaceSlag)),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(FlyAsh)),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(water)),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(Superplasticizer)),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(CoarseAggregate)),alpha=0.5)


ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(FineAggregate)),alpha=0.5)

ggplot(concrete,aes(x=row.names(concrete),y=CompressiveStrength))+geom_point(aes(colour=cut2(Age)),alpha=0.5)



names <- colnames(concrete)
names <- names[-length(names)]

featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")


index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + geom_point() + 
    theme_bw()

cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
    theme_bw()


#3
hist(concrete$Superplasticizer)

It is clear that there are plenty of zeros in this parameter so taking the log base 10 would yield infinities.


#4


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

grep("^IL",names(training))

preProcess(training[,grep("^IL",names(training))],method="pca",thresh=0.80)

Created from 251 samples and 12 variables

Pre-processing:
  - centered (12)
  - ignored (0)
  - principal component signal extraction (12)
  - scaled (12)

PCA needed 7 components to capture 80 percent of the variance


#5

# without preprocessing

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

grep("(^IL)|(^diagnosis)",names(training))

training <- training[, grep("(^IL)|(^diagnosis)",names(training))]
testing <- testing[ ,grep("(^IL)|(^diagnosis)",names(testing))]

model <- train(diagnosis ~ .,
data = training,
method = "glm"
)

plsClasses <- predict(model,newdata=testing)

confusionMatrix(data = plsClasses, testing$diagnosis)

Confusion Matrix and Statistics

          Reference
Prediction Impaired Control
  Impaired        2       9
  Control        20      51
                                         
               Accuracy : 0.6463         
                 95% CI : (0.533, 0.7488)
    No Information Rate : 0.7317         
    P-Value [Acc > NIR] : 0.96637    


# with preprocessing


ctrl <- trainControl(preProcOptions = list(thresh = 0.80))

model2 <- train(diagnosis ~ .,
data = training,
method = "glm",
preProcess="pca",
trControl=ctrl
)

plsClasses2 <- predict(model2,newdata=testing)

confusionMatrix(data = plsClasses2, testing$diagnosis)


Confusion Matrix and Statistics

          Reference
Prediction Impaired Control
  Impaired        3       4
  Control        19      56
                                          
               Accuracy : 0.7195          
                 95% CI : (0.6094, 0.8132)
    No Information Rate : 0.7317          
    P-Value [Acc > NIR] : 0.651780   



