Credit = read.csv(file = "crx_data.data",sep=",", header = FALSE, na.strings = "?")
names(Credit)[16]="Class"
summary(Credit)


Credit=na.omit(Credit)
summary(Credit)
dim(Credit)
install.packages("nnet")
library(nnet)
install.packages("e1071")
library(e1071)

#Logistic Regression
sub =sample(nrow(Credit),floor(nrow(Credit)*0.8))
train = Credit[sub,]
test = Credit[-sub,]
fit.lr=glm(Class~., data=train, family = binomial)
ProbL=predict(fit.lr, test, type="response")
PredL = rep(1:131)
PredL[ProbL>0.5]="+"
PredL[ProbL<=0.5]="-"
table(PredL,test$Class)
accuracyLR=sum(test$Class==PredL)/length(PredL)
accuracyLR

k=10
err=seq(1:10)
set.seed(1)
folds=sample(1:k,nrow(Credit),replace = TRUE)
for(i in 1:k)
{
  train=Credit[folds!=i,]
  test=Credit[folds==i,]
  fit.lr=glm(Class~., data=train, family = binomial)
  ProbL=predict(fit.lr, test, type="response")
  l=length(test$Class)
  PredL = rep(1:l)
  PredL[ProbL>0.5]="+"
  PredL[ProbL<=0.5]="-"
  accuracy=sum(PredL==test$Class)/length(PredL)
  err[i]=accuracy
}
err
ave=mean(err)
ave

#Random Forest
sub = sample(nrow(Credit),floor(nrow(Credit)*0.8))
train = Credit[sub,]
test = Credit[-sub,]
fit.rf=randomForest(Class~., data = train, ntree = 1000, mtry = 3)
PredT = predict(fit.rf,test)
table(PredT,test$Class)
accuracy.rf=sum(PredT == test$Class)/length(PredT)
accuracy.rf

k=10
err=seq(1:10)
folds=sample(1:k,nrow(Credit),replace = TRUE)
for(i in 1:k)
{
  train=Credit[folds!=i,]
  test=Credit[folds==i,]
  set.seed(1)
  fit.rf=randomForest(Class~., data = train, ntree = 1000, mtry = 3)
  PredT = predict(fit.rf,test)
  accuracy.rf=sum(PredT == test$Class)/length(PredT)
  err[i]=accuracy.rf
}
err
ave=mean(err)
ave


#Neural Network
fit.ann=nnet(Class~.,Credit,size = 15)
pred.ann=predict(fit.ann, Credit,type="class")
accuracyNN=sum(Credit$Class==pred.ann)/length(pred.ann)
accuracyNN

#10-folds CV for NN
k=10
err=seq(1:10)
folds=sample(1:k,nrow(Credit),replace = TRUE)
for(i in 1:k)
{
  train=Credit[folds!=i,]
  test=Credit[folds==i,]
  
  fit.ann=nnet(Class~.,train,size = 10,trace=FALSE)
  
  pred=predict(fit.ann,test,type="class")
  length(test$Class)
  accuracy=sum(pred==test$Class)/length(test$Class)
  err[i]=accuracy
}
err
ave=mean(err)
ave

#Support Vector Machine
fit.svm <- svm(Class~., data=Credit)
predS <- predict(fit.svm, Credit)
accuracy.svm=sum(predS == Credit$Class)/length(predS)
accuracy.svm

#10-folds CV for SVM
k=10
err=seq(1:10)
folds=sample(1:k,nrow(Credit),replace = TRUE)
for(i in 1:k)
{
  train=Credit[folds!=i,]
  test=Credit[folds==i,]
  
  fit.svm=svm(Class~.,data=train)
  
  pred=predict(fit.svm,test)
  length(test$Class)
  accuracy=sum(pred==test$Class)/length(test$Class)
  err[i]=accuracy
}
err
ave=mean(err)
ave
