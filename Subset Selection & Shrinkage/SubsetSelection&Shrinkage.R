NewsPop =  read.csv("./R/OnlineNewsPopularity/OnlineNewsPopularity.csv", sep=",")
View(NewsPop)
summary(NewsPop)
NewsPop = NewsPop[-c(1,2)]
View(NewsPop)
summary(NewsPop)
fac_col = c(12:17,30:37)
NewsPop[fac_col] = lapply(NewsPop[fac_col], factor)
View(NewsPop)
summary(NewsPop)
dim(NewsPop)

#FWD
NewsPop.model.fwd = regsubsets(shares~., data=NewsPop, nvmax=58, method="forward")
summary(NewsPop.model.fwd)
NewsPop.fwd.summary = summary(NewsPop.model.fwd)
NewsPop.fwd.summary$adjr2
NewsPop.fwd.summary$cp
NewsPop.fwd.summary$bic
plot(NewsPop.fwd.summary$adjr2)
plot(NewsPop.fwd.summary$cp)
plot(NewsPop.fwd.summary$bic)
which.max(NewsPop.fwd.summary$adjr2)
which.min(NewsPop.fwd.summary$cp)
which.min(NewsPop.fwd.summary$bic)
fwd.coef.46 = coef(NewsPop.model.fwd,46)
fwd.coef.28 = coef(NewsPop.model.fwd,28)
fwd.coef.9 = coef(NewsPop.model.fwd,9)
names(fwd.coef.46)
names(fwd.coef.28)
names(fwd.coef.9)


predict.regsubsets=function(object,newdata,id,...){
form=as.formula(object$call[[2]])
mat=model.matrix(form,newdata)
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi}
k=10
folds=sample(1:k,nrow(NewsPop),replace=TRUE)
cv.errors=matrix(NA,k,58, dimnames=list(NULL, paste(1:58)))
for(j in 1:k){
fwd=regsubsets(shares~.,data=NewsPop[folds!=j,],nvmax=58,method='forward')
for(i in 1:55){
  pred=predict(fwd,NewsPop[folds==j,],id=i)
cv.errors[j,i]=mean( (NewsPop$shares[folds==j]-pred)^2)}}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
fwd.coef.22 = coef(NewsPop.model.fwd,22)
names(fwd.coef.22)

#backward subset selection
NewsPop.model.bwd = regsubsets(shares~., data = NewsPop, nvmax = 58, method = "backward")
NewsPop.bwd.summary = summary(NewsPop.model.bwd)
NewsPop.bwd.summary
NewsPop.bwd.summary$adjr2
NewsPop.bwd.summary$cp
NewsPop.bwd.summary$bic

number_of_variables = rep(1:56)
plot(number_of_variables,NewsPop.bwd.summary$adjr2,main = "Adjust R Square")
plot(number_of_variables,NewsPop.bwd.summary$cp, main = "Cp")
plot(number_of_variables, NewsPop.bwd.summary$bic, main = "BIC")
which.max(NewsPop.bwd.summary$adjr2)
which.max(NewsPop.bwd.summary$cp)
which.max(NewsPop.bwd.summary$bic)

bwd.cof_39=coef(NewsPop.model.bwd,39)
bwd.cof_34=coef(NewsPop.model.bwd,34)
bwd.cof_10=coef(NewsPop.model.bwd,10)
names(bwd.cof_39)
names(bwd.cof_34)
names(bwd.cof_10)

###########Cross Validation###################
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

k=10
folds=sample(1:k,nrow(NewsPop),replace=TRUE)
cv.errors=matrix(NA,k,58, dimnames=list(NULL, paste(1:58)))
for(j in 1:k){
  fwd=regsubsets(shares~.,data=NewsPop[folds!=j,],nvmax=58,method='backward')
  for(i in 1:54){
    pred=predict(fwd,NewsPop[folds==j,],id=i)
    cv.errors[j,i]=mean( (NewsPop$shares[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)

bwd.cof_13=coef(NewsPop.model.bwd,13)
names(bwd.cof_13)


#########RIDGE REGRESSION#########################
NewsPop=read.csv(file = "C:/Users/mxs15/Desktop/OnlineNewsPopularity.csv",sep = ",",header = TRUE)
NewsPop = NewsPop[-c(1,2)]
fac_col = c(12:17,30:37)
NewsPop[fac_col] = lapply(NewsPop[fac_col], factor)
dim(NewsPop)

x=model.matrix(shares~.,NewsPop)[,-1]
y=NewsPop$shares
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha = 0,lambda = grid)
dim(coef(ridge.mod))
predict(ridge.mod,s=50,type = "coefficients")[1:59,]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha = 0,lambda = grid,thresh = 1e-2)
ridge.pred=predict(ridge.mod,s=5,newx = x[test,])
mean((ridge.pred-y.test)^2)

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx = x[test,])
mean((ridge.pred-y.test)^2)
ridge.model=glmnet(x,y,alpha = 0)
predict(ridge.model,type="coefficients",s=bestlam)[1:59,]



#lasso
x=model.matrix(shares~.,NewsPop)[,-1]
y=NewsPop$shares
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x,y,alpha=1,lambda=grid)
dim(coef(lasso.mod))
predict(lasso.mod,s=50,type = "coefficients")[1:59]

set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
lasso.mod=glmnet(x[train,],y[train],alpha = 1, lambda = grid, thresh = 1e-2)
lasso.pred=predict(lasso.mod,s=5,newx = x[test,])
mean((lasso.pred-y.test)^2)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
lasso.model=glmnet(x,y,alpha=1)
predict(lasso.model,type="coefficients",s=bestlam)[1:59,]
