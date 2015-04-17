set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
library("e1071")
set.seed(325)

model1 = svm(CompressiveStrength~.,data=training)

pred1 = predict(model1,newdata = testing)

#rmse
sqrt(sum((pred1-testing$CompressiveStrength)^2)/length(pred1))

##########################################################
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)

fit = bats(tstrain)

pred1 = (forecast(fit,h=(600-365)))

sum(testing$visitsTumblr<=pred1$upper[,2]&testing$visitsTumblr>=pred1$lower[,2])/length(testing$visitsTumblr)

###########################################################
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

modfit1 = train(CompressiveStrength~.,method="lasso",data=training)

pred1 = predict(modfit1, newdata=testing)

plot.enet(modfit1$finalModel, xvar="penalty", use.color=T)

#########################################################

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

modfit1 = train(diagnosis~.,method="rf",data=training)
modfit2 = train(diagnosis~.,method="gbm",data=training)
modfit3 = train(diagnosis~.,method="lda",data=training)

pred1 = predict(modfit1, newdata=testing)
pred2 = predict(modfit2, newdata=testing)
pred3 = predict(modfit3, newdata=testing)

sum(pred1==testing$diagnosis)/length(testing$diagnosis)
sum(pred2==testing$diagnosis)/length(testing$diagnosis)
sum(pred3==testing$diagnosis)/length(testing$diagnosis)

predDF = data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit = train(diagnosis~.,method="rf",data=predDF)
combPred = predict(combModFit,predDF)

sum(combPred==predDF$diagnosis)/length(combPred)

###############################################
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.test$y = factor(vowel.test$y)
vowel.train$y = factor(vowel.train$y)

set.seed(33833)

modfit = train(y~.,method="rf",data=vowel.train) 
modfit2 = train(y~.,method="gbm",data=vowel.train) 

pred1 = predict(modfit, newdata=vowel.test)
pred2 = predict(modfit2, newdata=vowel.test)

confusionMatrix(vowel.test$y,pred1)
sum(pred1==vowel.test$y)/length(vowel.test$y)

confusionMatrix(vowel.test$y,pred2)
sum(pred2==vowel.test$y)/length(vowel.test$y)

agree = pred1[pred1==pred2]
trueagree = vowel.test$y[pred1==pred2]
sum(agree==trueagree)/length(trueagree)

################################################

library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y=factor(vowel.train$y)
vowel.test$y=factor(vowel.test$y)

set.seed(33833)

modfit = train(y~.,method="rf",data=vowel.train) 
variableImp = varImp(modfit$finalModel)
order(variableImp)

#################################################
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)

modfit = train(chd~age+alcohol+obesity+tobacco+typea+ldl,method="glm",family="binomial",data=trainSA) 

trainpred = predict(modfit$finalModel,newdata=trainSA,type="response")
prediction = predict(modfit$finalModel,newdata=testSA,type="response")

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(testSA$chd,prediction)
missClass(trainSA$chd,trainpred)
################################################


library(pgmm)
data(olive)
olive = olive[,-1]

newdata = as.data.frame(t(colMeans(olive)))

modfit = train(Area~.,method="rpart",data=olive) 
modfit$finalModel = tree(Area~.,data=olive) 

outcome = predict(modfit$finalModel,newdata=newdata)

##############################################

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

L = segmentationOriginal$Case == "Train"
training = segmentationOriginal[L,]
testing  = segmentationOriginal[!L,]

set.seed(125)

modfit = train(Class~.,method="rpart",data=training) 


library(rattle)
fancyRpartPlot(modfit$finalModel)
testFit = predict(modfit,newdata=training)
