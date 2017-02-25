################# PARKINSON DATASET ################################
Parkinson = read.csv(file = "parkinsons.csv.data", header = TRUE)
Parkinson = Parkinson[,-1]
Parkinson$status = as.factor(Parkinson$status)
names(Parkinson)
sub = sample(nrow(Parkinson),floor(nrow(Parkinson)*0.8))

train = Parkinson[sub,]
test = Parkinson[-sub,]
dim(train)
dim(test)

#Logistic Regression
train.lr = train
parkinson.lr = glm(status~., data=train.lr, family = binomial)
parkinson.prob = predict(parkinson.lr,test,type = "response")
parkinson.prob
pred.lr = rep(1:39)
pred.lr[parkinson.prob>0.5]=1
pred.lr[parkinson.prob<=0.5]=0
table(pred.lr,test$status)
mean(pred.lr==test$status)

#LDA
train.lda = train[,-5]
parkinson.lda = lda(status~., data = train.lda)
parkinson.pred = predict(parkinson.lda,test)
names(parkinson.pred)
parkinson.class = parkinson.pred$class
table(parkinson.class, test$status)
mean( parkinson.pred$class==test$status)

#QDA
train.qda = train
parkinson.qda = qda(status~., data = train.qda)
QDA.pred = predict(parkinson.qda,test)
QDA.class = QDA.pred$class
table(QDA.class, test$status)
mean( QDA.class==test$status)

#Naive Bayes
install.packages("e1071")
library(e1071)
train.nb = train
train.nb = naiveBayes(status~., data = train.nb, laplace = 3)
NB.pred = predict(train.nb, test)
table(NB.pred,test$status)
mean( NB.pred==test$status)



################## GOAL DATASET ####################################
fieldgoal <- read.csv("./NFL FG/NFL FG/fieldgoal_data.dat", sep="")
goal = fieldgoal
names(goal) = c("distance", "result", "week")
View(goal)
summary(goal)
goal$result = as.factor(goal$result)
summary(goal)

sub =sample(nrow(goal),floor(nrow(goal)*0.8))

#LR
train = goal[sub,]
test = goal[-sub,]
goal.glm.model = glm(result~distance+week, data=train, family = binomial)
goal.glm.model
goal.prob = predict(goal.glm.model, test, type="response")
goal.pred = rep(1:190)
sample(goal.prob,10)
goal.pred[goal.prob>0.5]=1
goal.pred[goal.prob<=0.5]=0
table(goal.pred,test$result)
mean(goal.pred==test$result)

#LDA

goal.lda.model = lda(result~distance+week, data=train)
goal.lda.model
goal.pred = predict(goal.lda.model, test)
names(goal.pred)
goal.class=goal.pred$class
table(goal.class,test$result)
mean(goal.class==test$result)

#QDA

goal.qda.model = qda(result~distance+week, data=train)
goal.qda.model
goal.pred = predict(goal.qda.model, test)
names(goal.pred)
goal.class=goal.pred$class
table(goal.class,test$result)
mean(goal.class==test$result)


#NB

install.packages("e1071")
library(e1071)
goal.nb.model = naiveBayes(result~distance+week, data=train, laplace = 3)
goal.nb.model
goal.pred=predict(goal.nb.model, test)
table(goal.pred,test$result)
mean(goal.pred==test$result)
