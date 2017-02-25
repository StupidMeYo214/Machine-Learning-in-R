install.packages("splines")
library(splines)
abalone = read.csv(file="https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",header = FALSE,sep=",")
names(abalone)=c("Sex","Length","Diameter","Height","Whole Weight","Shucked Weight","VWGHT","Shell Weight","Rings")
summary(abalone)

#Polynomial Regression (degree=4)
k=10
err=seq(1:10)
folds=sample(1:k,nrow(abalone),replace = TRUE)
for(i in 1:k)
{
  train=abalone[folds!=i,]
  test=abalone[folds==i,]
  fit=lm(VWGHT~Diameter+Diameter^2+Diameter^3+Diameter^4,data=train)
  pred=predict(fit,test)
  mse.temp=mean((pred-test$VWGHT)^2)
  err[i]=mse.temp
}
err
mse4=mean(err)
mse4

#Polynomial Regression (degree=3)
k=10
err=seq(1:10)
folds=sample(1:k,nrow(abalone),replace = TRUE)
for(i in 1:k)
{
  train=abalone[folds!=i,]
  test=abalone[folds==i,]
  fit=lm(VWGHT~Diameter+Diameter^2+Diameter^3,data=train)
  pred=predict(fit,test)
  mse.temp=mean((pred-test$VWGHT)^2)
  err[i]=mse.temp
}
err
mse3=mean(err)
mse3

#Polynomial Regression (degree=2)
k=10
err=seq(1:10)
folds=sample(1:k,nrow(abalone),replace = TRUE)
for(i in 1:k)
{
  train=abalone[folds!=i,]
  test=abalone[folds==i,]
  fit=lm(VWGHT~Diameter+Diameter^2,data=train)
  pred=predict(fit,test)
  mse.temp=mean((pred-test$VWGHT)^2)
  err[i]=mse.temp
}
err
mse2=mean(err)
mse2

#Polynomial Regression (degree=1)
k=10
err=seq(1:10)
folds=sample(1:k,nrow(abalone),replace = TRUE)
for(i in 1:k)
{
  train=abalone[folds!=i,]
  test=abalone[folds==i,]
  fit=lm(VWGHT~Diameter,data=train)
  pred=predict(fit,test)
  mse.temp=mean((pred-test$VWGHT)^2)
  err[i]=mse.temp
}
err
mse1=mean(err)
mse1

mse1
mse2
mse3
mse4

#Cubic Spline
k=10
err=seq(1:10)
folds=sample(1:k,nrow(abalone),replace = TRUE)
for(i in 1:k)
{
  train=abalone[folds!=i,]
  test=abalone[folds==i,]
  fit=lm(VWGHT~bs(Diameter, knots=c(.2, .4, .5)), data=train)
  pred=predict(fit,test)
  mse.temp=mean((pred-test$VWGHT)^2)
  err[i]=mse.temp
}
err
mse.cs=mean(err)
mse.cs

#natural splines
k=10
folds=sample(1:k,nrow(Abalone),replace = TRUE)
#degree of freedom = 2
err2=seq(1:10)
for(i in 1:k)
{
  train2=Abalone[folds!=i,]
  test2=Abalone[folds==i,]
  fit.nspline2 = lm(VWGHT~ns(Diameter,df=2),data=Abalone)
  pred2=predict(fit.nspline2,test2)
  mse.temp2=mean((pred2-test2$VWGHT)^2)
  err2[i]=mse.temp2
}
err2
mse2=mean(err2)
mse2
#degree of freedom = 4
err4=seq(1:10)
for(i in 1:k)
{
  train4=Abalone[folds!=i,]
  test4=Abalone[folds==i,]
  fit.nspline4 = lm(VWGHT~ns(Diameter,df=4),data=Abalone)
  pred4=predict(fit.nspline4,test4)
  mse.temp4=mean((pred4-test4$VWGHT)^2)
  err4[i]=mse.temp4
}
err4
mse4=mean(err4)
mse4
#degree of freedom = 6
err6=seq(1:10)
for(i in 1:k)
{
  train6=Abalone[folds!=i,]
  test6=Abalone[folds==i,]
  fit.nspline6 = lm(VWGHT~ns(Diameter,df=6),data=Abalone)
  pred6=predict(fit.nspline6,test6)
  mse.temp6=mean((pred6-test6$VWGHT)^2)
  err6[i]=mse.temp6
}
err6
mse6=mean(err6)
mse6
#degree of freedom = 8
err8=seq(1:10)
for(i in 1:k)
{
  train8=Abalone[folds!=i,]
  test8=Abalone[folds==i,]
  fit.nspline8 = lm(VWGHT~ns(Diameter,df=8),data=Abalone)
  pred8=predict(fit.nspline8,test8)
  mse.temp8=mean((pred8-test8$VWGHT)^2)
  err8[i]=mse.temp8
}
err8
mse8=mean(err8)
mse8

min(mse2,mse4,mse6,mse8)

diamLims=range(Diameter)
diamLims = diamLims*100
diam.grid=seq(diamLims[1],diamLims[2])
diam.grid = diam.grid/100
#draw line for natural spline
fit.nspline = lm(VWGHT~ns(Diameter,df=4),data=Abalone)
pred_ns = predict(fit.nspline,newdata = list(Diameter=diam.grid))
lines(diam.grid,pred_ns,col="navy",lwd=2)

#smooth splines
#degree of freedom = 3
err3=seq(1:10)
for(i in 1:k)
{
  train3=Abalone[folds!=i,]
  test3=Abalone[folds==i,]
  fit.sspline3 = smooth.spline(train3$Diameter,train3$VWGHT, df=3)
  pred_ss3=predict(fit.sspline3,test3$Diameter)
  mse.temp3=mean((pred_ss3$y-test3$VWGHT)^2)
  err3[i]=mse.temp3
}
err3
mse3=mean(err3)
mse3
#degree of freedom = 5
err5=seq(1:10)
for(i in 1:k)
{
  train5=Abalone[folds!=i,]
  test5=Abalone[folds==i,]
  fit.sspline5 = smooth.spline(train5$Diameter,train5$VWGHT, df=5)
  pred_ss5=predict(fit.sspline5,test5$Diameter)
  mse.temp5=mean((pred_ss5$y-test5$VWGHT)^2)
  err5[i]=mse.temp5
}
err5
mse5=mean(err5)
mse5
#degree of freedom = 7
err7=seq(1:10)
for(i in 1:k)
{
  train7=Abalone[folds!=i,]
  test7=Abalone[folds==i,]
  fit.sspline7 = smooth.spline(train7$Diameter,train7$VWGHT, df=7)
  pred_ss7=predict(fit.sspline7,test7$Diameter)
  mse.temp7=mean((pred_ss7$y-test7$VWGHT)^2)
  err7[i]=mse.temp7
}
err7
mse7=mean(err7)
mse7
#degree of freedom = 9
err9=seq(1:10)
for(i in 1:k)
{
  train9=Abalone[folds!=i,]
  test9=Abalone[folds==i,]
  fit.sspline9 = smooth.spline(train9$Diameter,train9$VWGHT, df=9)
  pred_ss9=predict(fit.sspline9,test9$Diameter)
  mse.temp9=mean((pred_ss9$y-test9$VWGHT)^2)
  err9[i]=mse.temp9
}
err9
mse9=mean(err9)
mse9

min(mse3,mse5,mse7,mse9)
#draw line for smoothing spline
fit.sspline = smooth.spline(Diameter,VWGHT,cv=TRUE)
pred_sspline = predict(fit.sspline, newata=list(Diameter=diam.grid))
lines(pred_sspline,col="orange",lwd=2)


#plot
plot(VWGHT~Diameter)

diam.range=range(Diameter)
diam.range=diam.range*100
diam.range
diam.grid=seq(diam.range[1],diam.range[2])
diam.grid=diam.grid/100
pred = predict(fit,newdata = list(Diameter=diam.grid),se=T)

#cubic spline
lines(diam.grid,pred$fit,lwd=2,col="red")
lines(diam.grid,pred$fit +2*pred$se,lty="dashed", col="red")
lines(diam.grid,pred$fit - 2*pred$se,lty="dashed", col="red")
#natrual spline
fit.nspline = lm(VWGHT~ns(Diameter,df=4),data=abalone)
pred_ns = predict(fit.nspline,newdata = list(Diameter=diam.grid))
lines(diam.grid,pred_ns,col="yellow",lwd=2)
#smoothing spline
fit.sspline = smooth.spline(Diameter,VWGHT,cv=TRUE)
pred_sspline = predict(fit.sspline, newata=list(Diameter=diam.grid))
lines(pred_sspline,col="green",lwd=2)

#add legend
labels=c("cubic spline","natrual spline","smoothing spline")
legend("topleft",labels,lwd=2,col = c("red","yellow","green"),lty = c(1,1,1))

