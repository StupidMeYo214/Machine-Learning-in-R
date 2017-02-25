#import data
Housing = read.csv(file = "dataset_in_wide_format.csv", sep = ",", header = TRUE)
summary(Housing)
myData=Housing
#data clean
myData=myData[-c(15)]
myData=myData[-c(1,3:6,8,10,12,13,15,16,21:23,25,27,28)]
names(myData)
summary(myData)

sub = sample(nrow(myData),floor(nrow(myData)*0.8))
train = myData[sub,]
test = myData[-sub,]
dim(train)
dim(test)
dim(myData)

#decision tree
install.packages("tree")
library(tree)

tree.house=tree(train$pctchange~.,train,split = "deviance")
tree.pred=predict(tree.house,test,type="vector")
mse_1=mean((tree.pred-test$pctchange)^2)
mse_1

plot(tree.house)
text(tree.house,pretty = 0)
tree.house


tree.house.cv=cv.tree(tree.house, FUN = prune.tree)
tree.house.cv
plot(tree.house.cv$size,tree.house.cv$dev,type = "b")
tree.house.prune = prune.tree(tree.house,best = 5)
plot(tree.house.prune)
text(tree.house.prune)

tree.prune.pred=predict(tree.house.prune,test)
test$pctchange
mse_2=mean((tree.prune.pred-test$pctchange)^2)
mse_2

#bagging
install.packages("ipred")
library(ipred)


bagging.house = bagging(pctchange~., data=train,nbagg=20 )
bagging.house
summary(bagging.house)
bagging.house.pred = predict(bagging.house, test)
bagging.house.pred
test$pctchange
mse_3 = mean((bagging.house.pred-test$pctchange)^2)
mse_3

k=10
err=seq(1:10)
folds=sample(1:k,nrow(myData),replace = TRUE)
for(i in 1:k)
{
  train=myData[folds!=i,]
  test=myData[folds==i,]
  bagging.house = bagging(pctchange~., data=train,nbagg=20 )
  pred=predict(bagging.house, test)
  mse.temp=mean((pred-test$pctchange)^2)
  err[i]=mse.temp
}
err
mse_3=mean(err)
mse_3


#boosting
install.packages("gbm")
library(gbm)

set.seed(1)
boosting.house = gbm(pctchange~.,data=train, distribution = "gaussian", n.trees = 200, shrinkage = 0.05, cv.folds = 10)
boosting.house.pred = predict(boosting.house,test)
test$pctchange
mean((boosting.house.pred-test$pctchange)^2)

k=10
err=seq(1:10)
folds=sample(1:k,nrow(myData),replace = TRUE)
for(i in 1:k)
{
  train=myData[folds!=i,]
  test=myData[folds==i,]
  set.seed(1)
  boosting.house = gbm(pctchange~.,data=train, distribution = "gaussian", n.trees =700, shrinkage = 0.01, cv.folds = 10)
  pred=predict(boosting.house, test)
  mse.temp=mean((pred-test$pctchange)^2)
  err[i]=mse.temp
}
err
mse_4=mean(err)
mse_4

#random forest
install.packages("randomForest")
library(randomForest)


rf.house = randomForest(pctchange~., data = train, ntree = 1000, mtry = 3)
rf.house.pred = predict(rf.house,test)
mean((rf.house.pred-test$pctchange)^2)


k=10
err=seq(1:10)
folds=sample(1:k,nrow(myData),replace = TRUE)
for(i in 1:k)
{
  train=myData[folds!=i,]
  test=myData[folds==i,]
  set.seed(1)
  rf.house = randomForest(pctchange~., data = train, ntree = 1000, mtry = 3)
  pred=predict(rf.house,test)
  mse.temp=mean((pred-test$pctchange)^2)
  err[i]=mse.temp
}
err
mse_5=mean(err)
mse_5