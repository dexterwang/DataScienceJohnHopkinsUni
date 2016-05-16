how you built your model, 
how you used cross validation, 
what you think the expected out of sample error is, 
and why you made the choices you did. 


library(caret)
library(ggplot2)


setwd("C:/D/R/Practical Machine Learning/Week4 Project")

training <- read.csv("./pml-training.csv",header=TRUE,stringsAsFactors=FALSE)

submit_testing <- read.csv("./pml-testing.csv",header=TRUE,stringsAsFactors=FALSE)

#data processing

#data cleaning

str(training)
dim(training)


# replace "#DIV/0!" with NA
training[!is.na(training) &  training=="#DIV/0!"]  <- NA


# remove the columns which are mostly NA or blank values
na_percent <-sapply(training, function(y) sum(length(which(is.na(y)|y=="")))/length(y) )

na_percent <- data.frame(na_percent)

na_percent

remove_index <- which(na_percent>0.97)

# remove the id/timestamp columns

remove_index <- c(remove_index,grep("X|timestamp|user_name", names(training)))

training <- training[,-remove_index]

# after removing useless columns
str(training)
dim(training)

# split training data into training (80%) / testing set (20%) 

training$classe <- as.factor(training$classe) 

training$new_window <- as.factor(training$new_window)


set.seed(60515)

inTrain <- createDataPartition(training$classe, p = 0.8)[[1]]

testing <- training[-inTrain,]

training <- training[inTrain,]

dim(training)
dim(testing)



set.seed(60515)




tuneRF(training[,-length(training[1,])], training[,length(training[1,])], mtryStart=2, ntreeTry=500, stepFactor=2, improve=0.005,trace=TRUE, plot=TRUE, doBest=TRUE)



#feature selection

training<-training[,-which(names(training) %in% c("num_window"))]


ttraining<-training
training<- ttraining[,2:(length(ttraining[1,])-1)]

library(mlbench)

# calculate correlation matrix
correlationMatrix <- cor(training)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

unselected<- names(training[,highlyCorrelated])

selected<-names(training[,-highlyCorrelated])

training_2 <- training[,selected]

correlationMatrix_2 <- cor(training_2)

highlyCorrelated_2 <- findCorrelation(correlationMatrix_2, cutoff=0.75)

highlyCorrelated_2


#training<- cbind(ttraining[,selected],ttraining[,c(1,length(ttraining))])



#select 34 variables
names(cbind(ttraining[,selected],ttraining[,c(1,length(ttraining))]))

#drop 21 variables
unselected

tuneRF(training[,-length(training)], training[,length(training)], mtryStart=2, ntreeTry=500, stepFactor=2, improve=0.005,trace=TRUE, plot=TRUE, doBest=TRUE)

set.seed(60515)
model<- randomForest(classe~.,data=training,importance=TRUE,mtry=8)

set.seed(60515)
model2<- randomForest(classe~.,data=training,importance=TRUE,mtry=16)

set.seed(60515)
model3<- randomForest(classe~.,data=training,importance=TRUE,mtry=6)

set.seed(60515)
model4<- randomForest(classe~.,data=training,importance=TRUE,mtry=4)


pd <- predict(model,testing)

pd2 <- predict(model2,testing)

pd3 <- predict(model3,testing)

pd4 <- predict(model4,testing)


acc <- sum(testing$classe==pd)/length(testing$classe)

acc2 <- sum(testing$classe==pd2)/length(testing$classe)

acc3 <- sum(testing$classe==pd3)/length(testing$classe)

acc4 <- sum(testing$classe==pd4)/length(testing$classe)

1-acc

1-acc2

1-acc3

1-acc4


submit_testing$new_window <- as.factor(submit_testing$new_window)

levels(submit_testing$new_window) <- levels(training$new_window)

hist(t$yaw_belt,col=heat.colors(t$classe))

hist(t$yaw_belt, col=heat.colors(t$classe))

library(ggplot2)

ggplot(data=t, aes(num_window)) + geom_histogram(col="red",aes(fill=classe),bins=100)

ggplot(data=t, aes(yaw_belt)) + geom_histogram(col="red",aes(fill=classe),bins=100)

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram(breaks=seq(20, 50, by =2), 
                 col="red", 
                 aes(fill=..count..))

varImpPlot(model)


cols <- names(submit_testing)

dif_c <- c()

for(n in names(submit_testing))
{
	if(class(submit_testing[,n])!=class(training[,n]))
	{
		dif_c <- c(dif_c,n)
	}
}

sapply(training[,dif_c], class)
sapply(submit_testing[,dif_c], class)

for(n in dif_c)
{
	submit_testing[,n]<-as.numeric(submit_testing[,n])
}

submit_testing[,dif_c]

cols<-intersect(names(training),names(submit_testing))

submit_testing<-submit_testing[,cols]


data_types_training <- data.frame(varname=names(training),class= sapply(training, class))

data_types_testing <- data.frame(varname=names(submit_testing),class= sapply(submit_testing, class))

cols <-intersect(data_types_training$varname,data_types_testing$varname)

data_types_training<-data_types_training[data_types_training$varname %in% cols,]

data_types_testing<-data_types_testing[data_types_testing$varname %in% cols,]

data_types_testing[data_types_testing$class!=data_types_training$class,"varname"]


cols



submit_testing <- submit_testing[,-remove_index]

submit_testing <-submit_testing[,-55]

sapply(training, class)

sapply(submit_testing, class)==sapply(training, class)[1:54]

cc<- c("magnet_dumbbell_z","magnet_forearm_y","magnet_forearm_z")

sapply(submit_testing[,cc], class)==sapply(training[,cc], class)

magnet_dumbbell_z

magnet_forearm_y

magnet_forearm_z 

pd_final <- predict(model,submit_testing)

pd_final

1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E

 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
 B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
Levels: A B C D E




er <-errorest(classe ~ ., data = training, model = model)
er$error

# force predict to return class labels only
mypredict.lda <- function(object, newdata)
  predict(object, newdata = newdata)$class

mm<- model
errorest(classe ~ ., data=training, model=rf, estimator = "cv", predict= mypredict.lda)  


control <- trainControl(method = "cv", number = 4)

model <- train(classe~ .,data=training,trControl=control,method="rf",prox=TRUE,importance=TRUE,tuneGrid = data.frame(mtry = 8))



median(model$err.rate[,"OOB"])



#Predict only using num_window


#looping all different settings
num_var <- c(54,33)
num_tree <- c(100,200,500,1000)
sample_var <- c(4,6,8,16)

performance_matrix <- data.frame(num_var=NA, ntree=NA,mty=NA,OOB=NA,testing_error=NA,run_time=NA)
performance_matrix<- performance_matrix[-1,]

for (i in num_var) {
	if(num_var==33){
		t<-training[,selected]
	}	
	else
	{
		t<-training
	}
  
	for(j in num_tree){
		for(k in sample_var){
			#start clock
			ptm <- proc.time()
			set.seed(60515)
			md<- randomForest(classe~.,data=t,mtry=k,ntree=j)

			t_elap<- (proc.time()-ptm)[3]
			#end clock
			p <- predict(md,testing)

			ac <- sum(testing$classe==p)/length(testing$classe)

			ac <- 1-ac 

			oob<-median(md$err.rate[,"OOB"])

			result <- c(length(t[1,]),j,k,oob,ac,t_elap)
			performance_matrix <- rbind(performance_matrix,result)
		}
	}
  
}







control <- trainControl(method = "cv", number = 4)

model <- train(classe~ .,data=training,trControl=control,method="rf",prox=TRUE,importance=TRUE,do.trace=TRUE)


model <- train(classe~ .,data=training,method="rf",prox=TRUE,importance=TRUE,ntree=1,do.trace=TRUE)


rf<- randomForest(classe~.,data=training,importance=TRUE,do.trace=TRUE)

fit=randomForest(factor(Y)~., data=df)

randomForest(x, y=NULL,  xtest=NULL, ytest=NULL, ntree=500,
             mtry=if (!is.null(y) && !is.factor(y))
             max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
             replace=TRUE, classwt=NULL, cutoff, strata,
             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
             nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
             maxnodes = NULL,
             importance=FALSE, localImp=FALSE, nPerm=1,
             proximity, oob.prox=proximity,
             norm.votes=TRUE, do.trace=FALSE,
             keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
             keep.inbag=FALSE, ...)

training[is.na(training) ] 

str(training)
tuneRF(training[,-55], training[,55], mtryStart=2, ntreeTry=500, stepFactor=2, improve=0.005,trace=TRUE, plot=TRUE, doBest=TRUE)

sapply(training, function(x) sum(is.na(x)))

model

varImp(model)


importance <- varImp(rf)



# summarize importance
print(importance)
# plot importance
plot(importance)


importance$mean <- apply(importance,1,mean)

importance$varname <- row.names(importance)

library(dplyr)
importance <- importance %>% arrange(desc(mean))





# remove highly correlated columns

ttraining<-training
training<- training[,2:54]

library(mlbench)

# calculate correlation matrix
correlationMatrix <- cor(training[,2:54])
correlationMatrix <- cor(training)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


h1<-names(training[,-highlyCorrelated])


highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)


h2<-names(training[,-highlyCorrelated])

training_2 <- training[,h2]

correlationMatrix_2 <- cor(training_2)

highlyCorrelated_2 <- findCorrelation(correlationMatrix_2, cutoff=0.75)


h2[!h2 %in% h1 ]

cor_selected <- intersect(h1,h2)

correlationMatrix_2 <- cor(training[,cor_selected[2:23]])

correlationMatrix_3 <- cor(training[,h2[2:36]])

highlyCorrelated_3 <- findCorrelation(correlationMatrix_3, cutoff=0.75)

print(correlationMatrix_2)

highlyCorrelated_2 <- findCorrelation(correlationMatrix_2, cutoff=0.75)

print(highlyCorrelated_2)


sub_training_23 <- training[,importance_23]

correlationMatrix_2[,c(13,22,8,5)]

correlationMatrix_3[,c(1,22,5,31,13,14,11)]


# calculate correlation matrix
correlationMatrix_23 <- cor(sub_training_23[,1:23])
# summarize the correlation matrix
print(correlationMatrix_23)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)




featurePlot(x=iris[,1:4], y=iris[,5], plot="pairs", auto.key=list(columns=3))

featurePlot(x=sub_training_23, y=training[,55], plot="pairs", auto.key=list(columns=5))










# rank variable by importance 

# ensure results are repeatable
set.seed(7)



# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(classe~., data=training, method="rf", trControl=control,importance=TRUE,ntree=50,do.trace=TRUE)
# estimate variable importance
importance <- varImp(model, scale=FALSE)



# summarize importance
print(importance)
# plot importance
plot(importance)









#feature selection


# ensure the results are repeatable
set.seed(7)


# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(training[,1:54], training[,55], sizes=c(1:54), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))





t <-apply(correlationMatrix,2, function(y) which(y>0.5&y!=1) )











# resources



#http://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/

#https://rpubs.com/mtaufeeq/PML_Project

#http://stats.stackexchange.com/questions/147758/unstable-variable-importance-ranking

#http://www.redbrick.dcu.ie/~noel/R_classification.html

#http://www.r-bloggers.com/variable-importance-plot-and-variable-selection/

#http://stats.stackexchange.com/questions/53240/practical-questions-on-tuning-random-forests

#http://machinelearningmastery.com/feature-selection-with-the-caret-r-package/

#http://caret.r-forge.r-project.org/featureselection.html

#https://rpubs.com/dandybits/ml4wle

#https://www.quora.com/What-is-the-out-of-bag-error-in-Random-Forests

