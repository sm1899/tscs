#pca
View(USArrests)
apply(USArrests, 2, mean)#Row means
apply(USArrests, 2, var)# Row variances
#PCA calculation
PCA= prcomp(USArrests, scale = TRUE)##PCA calculation
PCA$rotation ##Getting the principal components
biplot(PCA)
# getting SD/variance of each principal component
PCA$sdev
pr.var=PCA$sdev^2
pr.var#getting Variation of each principal component
screeplot(PCA, npcs = 24, type = "lines")
#Proportion of variation.
pve =pr.var / sum(pr.var)
pve
#
par(mar=c(1,1,1,1))
par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b") ##Plotting of PVE
plot(cumsum(pve), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = "b") ##Plotting of cummulative PVE

#PCA Regression Analysis
library(ISLR2)
library(pls)
set.seed(1)
str(Hitters)
View(Hitters)
Hitters=na.omit(Hitters)
x = model.matrix(Salary ~ ., Hitters)[, -1] 
y = Hitters$Salary
train = sample(1:nrow(x), nrow(x) *(2/3)) 
test = (-train)
y.test = y[test]
pcr.fit= pcr(Salary ~ ., data = Hitters, subset = train,
                 scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
#Min no. of PCA components
min.pcr = which.min( MSEP( pcr.fit )$val[1,1, ] ) - 1
min.pcr##Subtracting -1 because 1st component is coming as an intercept model
coef(pcr.fit, ncomp = min.pcr)
#Plotting of coefficients
pcr.fit$coefficients
#coef.mat = matrix(NA, 19, 19)
#for(i in 1:19){
#  coef.mat[,i] = pcr.fit$coefficients[,,i]
#}
#plot(coef.mat[1,], type = 'l', ylab = 'Coefficients', 
#     xlab = 'Number of components',
#     ylim = c(min(coef.mat), max(coef.mat)))
#for(i in 2:19){
#  lines(coef.mat[i,], col = i)
#}
#abline(v = min.pcr, lty = 3)
###########  Prediction #######
pcr.pred=predict(pcr.fit,x[test ,],ncomp =5)
mean((pcr.pred-y.test)^2)
pcr.fit = pcr(y~ x, scale = TRUE, ncomp = 5)
summary(pcr.fit)
# Scree Plot 
#PVE <- rep(NA,19)
#for(i in 1:19){ PVE[i]<- sum(pcr.fit$Xvar[1:i])/pcr.fit$Xtotvar }
#barplot( PVE, names.arg = 1:19, main = "scree plot", 
#         xlab = "number of PCs", 
#         ylab = "proportion of variance explained" )












#sheet2
setwd("/Users/Local Drive/Teaching/2022/SMRA/R-CODES")
library(ISLR2)
View(Credit)
str(Credit)

names(Credit)
dim(Credit)
sum(is.na(Credit))
Credit=na.omit(Credit)

#best subset selection 

library(leaps)
best.model=regsubsets(Balance~.,Credit, nvmax=11)
reg.summary=summary(best.model)
reg.summary
coef(best.model,1)
coef(best.model,2)
coef(best.model,3)
coef(best.model,4)
coef(best.model,6)
names(reg.summary)
reg.summary$adjr2
plot(reg.summary$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
coef(best.model,7)
points (7, reg.summary$adjr2[7] , col = "red", cex = 2,
        pch = 20)
#######################################
 par(mfrow = c(2, 2))
plot(reg.summary$rss , xlab = "Number of Variables",
       ylab = "RSS", type = "l")
plot(reg.summary$adjr2 , xlab = "Number of Variables",
       ylab = "Adjusted RSq",type="l")
 which.max(reg.summary$adjr2)
points (11, reg.summary$adjr2 [11] , col = "red", cex = 2,
          pch = 20)
####
reg.summary$bic
which.min(reg.summary$bic)
coef(best.model,4)
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC",type="l")
points (4, reg.summary$adjr2[4] , col = "red", cex = 2,
        pch = 20)
# Forward Selection 
regfit.fwd <- regsubsets(Balance~., data = Credit ,
                         nvmax = 11, method = "forward")
fwd.summary=summary(regfit.fwd)
fwd.summary
coef(regfit.fwd,1)
coef(regfit.fwd,2)
coef(regfit.fwd,3)
coef(regfit.fwd,4)
coef(regfit.fwd,5)
coef(regfit.fwd,6)
coef(regfit.fwd,7)
fwd.summary$adjr2
plot(fwd.summary$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq")
which.max(fwd.summary$adjr2)
points (7, fwd.summary$adjr2 [7] , col = "red", cex = 2,
        pch = 20)
######
plot(reg.summary$cp, xlab = "Number of Variables",
         ylab = "Cp", type = "l")
 which.min(reg.summary$cp)
points (10, reg.summary$cp[10] , col = "red", cex = 2,
          pch = 20)

#Backward Selection 

regfit.bwd <- regsubsets(Balance~., data = Credit ,
                         nvmax = 11, method = "backward")
bwd.summary=summary(regfit.bwd)
bwd.summary
coef(regfit.bwd,11)
coef(regfit.bwd,10)
coef(regfit.bwd,9)
coef(regfit.bwd,8)
coef(regfit.bwd,7)
coef(regfit.bwd,6)
bwd.summary$adjr2
plot(bwd.summary$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq",type="l")
which.max(bwd.summary$adjr2)
points (6, bwd.summary$adjr2 [6] , col = "red", cex = 2,
        pch = 20)

#Sequential stepwise  Selection

regfit.seq <- regsubsets(Balance~., data = Credit ,
                         nvmax = 11, method = "seqrep")
seq.summary=summary(regfit.seq)
seq.summary
coef(regfit.seq,1)
coef(regfit.seq,2)
coef(regfit.seq,3)
coef(regfit.seq,4)
coef(regfit.seq,5)
coef(regfit.seq,7)
seq.summary$adjr2
plot(seq.summary$adjr2 , xlab = "Number of Variables",
     ylab = "Adjusted RSq",type="l")
which.max(seq.summary$adjr2)
points (7, seq.summary$adjr2 [7] , col = "red", cex = 2,
        pch = 20)
##########
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points (6, reg.summary$cp[6] , col = "red", cex = 2,
        pch = 20)
#########
plot(reg.summary$bic , xlab = "Number of Variables",
     ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points (4, reg.summary$bic [4], col = "red", cex = 2,
        pch = 20)

#Validation set and cross-validation set approach
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Credit),rep=T)
train
test=(!train)
train.mat=model.matrix(Balance~., data=Credit[train, ])
train.mat
test.mat=model.matrix(Balance~., data=Credit[test, ])
test.mat
##best subset for train data
library(leaps)
regfit.best=regsubsets(Balance~.,data=Credit[train, ],nvmax=11, method="forward")
# Compute the test MSE using validation set approach 
test.mse=rep(NA, 11)
for (i in 1:11){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[ ,names(coefi)]%*%coefi
  test.mse[i]=mean((Credit$Balance[test]-pred)^2)
}
test.mse
which.min(test.mse)
coef(regfit.best,7)##best model according to validation set
par(mfrow=c(1,1))
plot(test.mse)
#########  Creating the predict function ###########
predict.regsubsets=function(object,newdata,id){ 
##extract the same structure as the object has
form=as.formula(object$call[[2]])
#do the same analysis as like object, with the new data
mat=model.matrix(form,newdata)
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi
}

sm=summary(best.fit)
sm$obj$call[[3]]
#Compute the test MSE using cross-validation set approach 
k=10
n=nrow(Credit)
set.seed(1)
folds=sample(rep(1:k, length=n))
folds
cv.errors=matrix(NA,k, 11, dimnames = list(NULL,paste(1:11)))
cv.errors
#######
for(j in 1:k){ 
  best.fit=regsubsets(Balance~.,
                      data=Credit[folds!=j,],
                      nvmax=11)
for(i in 1:11){ 
pred=predict(best.fit,Credit[folds==j,],id=i) 
cv.errors[j,i]=mean( (Credit$Balance[folds==j]-pred)^2)
              }
}
##############
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
which.min(mean.cv.errors)
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b")
reg.best=regsubsets(Balance~.,data=Credit, nvmax=11)
coef(reg.best,6)

#Ridge Regression 

grid=10^seq(10,-2,length=100)# Create a grid of lambda values
grid## See the all lambda values
library(glmnet)
View(Credit)
x=model.matrix(Balance~.,Credit)[,-1]#Extract the X matrix
y=Credit$Balance#Extract the response vector
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)# Ridge regression for all grid values
ridge.mod$lambda[45] # Lambda value corresponding to 45-th grid value
coef(ridge.mod)[, 45] # Coefficient values corresponding to 45th Lambda value
#dim(coef(ridge.mod))# dimension of the coefficient matrix
coef(ridge.mod)[, 34] # Coefficient values corresponding to 34-th grid value

# Estimation of the TEST MSE using Validation set

x=model.matrix(Balance~.,Credit)[,-1]#Extract the X matrix
y=Credit$Balance#Extract the response vector
grid=10^seq(10,-4,length.out=99)
grid=c(grid,0)
grid
set.seed(18)
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(Balance~., data=Credit[train, ])## Train data without response
test=(-train)
test.mat=model.matrix(Balance~., data=Credit[test, ])## Test data without response
test.mat
y.test=y[test]##Test responses
##Fit the regression model, for all lambda, for the train data
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
plot(ridge.mod,xvar="lambda")
##### TEST MSE for lambda[1]##########
ridge.pred1=predict(ridge.mod,s=ridge.mod$lambda[1] ,newx=x[test,])
mean((ridge.pred1-y.test)^2)#Test MSE for lambda[1]
predict(ridge.mod,s=ridge.mod$lambda[1] ,type="coefficients")[1:12,]
#####  Test MSE for lambda[25]  #########
ridge.pred2=predict(ridge.mod,s=ridge.mod$lambda[25] ,newx=x[test,]) 
mean((ridge.pred2-y.test)^2)
predict(ridge.mod,s=ridge.mod$lambda[25] ,type="coefficients")[1:12,]##The model
########## Test MSE for lambda[100]=0
ridge.pred3=predict(ridge.mod,s=ridge.mod$lambda[100],newx=x[test,], exact=T) 
mean((ridge.pred3-y.test)^2)
predict(ridge.mod,s=ridge.mod$lambda[100],type="coefficients")[1:12,]

#Choosing best lambda using 5-folds cross validation Method 
#based on train data 

##Note: lambda=0 case is not included in  cv.glmnet function. See the code of
##cv.glmnet
set.seed(16)
x=model.matrix(Balance~.,Credit)[,-1]#Extract the X matrix
y=Credit$Balance#Extract the response vector
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(Balance~., data=Credit[train, ])## Train data without response
test=(-train)
test.mat=model.matrix(Balance~., data=Credit[test, ])## Test data without response
y.test=y[test]##Test responses
##cv.glmnet has some dafault lambda values; 
#we may also set our own lambda values through grid
library(glmnet)
cv.out=cv.glmnet(x[train,],y[train],alpha=0, nfolds=5)
plot(cv.out)
#Choosing best Lambda 
 best.lambda=cv.out$lambda.min
best.lambda
# TEST MSE corresponding to best lambda
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=best.lambda, thresh=1e-12)
ridge.pred4=predict(ridge.mod,s=best.lambda,newx=x[test,]) 
mean((ridge.pred4-y.test)^2)
########### Refit the model using the full data by using the lambda chosen
#by cross-validation technique
Final_ridge=glmnet(x,y,alpha=0)
predict(Final_ridge,type="coefficients",s=best.lambda)[1:12,]

#THE LASSO

library(glmnet)
View(Credit)
x=model.matrix(Balance~.,Credit)[,-1]#Extract the X matrix
x
y=Credit$Balance#Extract the response vector
grid=10^seq(10,-2,length=100)# Create a grid of lambda values
grid## See the all lambda values
lasso.mod=glmnet(x,y,alpha=1,lambda=grid, thresh=1e-12)
lasso.mod$lambda[1] # Lambda value corresponding to 45-th grid value
dim(coef(lasso.mod))# dimension of the coefficient matrix
coef(lasso.mod)[, 45] # Coefficient values corresponding to 45th Lambda value
coef(lasso.mod)[, 100] # Coefficient values corresponding to 2nd lambda value

#####  Estimation of the TEST MSE using Validation set

set.seed(19)
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(Balance~., data=Credit[train, ])## Train data without response
test=(-train)
test.mat=model.matrix(Balance~., data=Credit[test, ])## Test data without response
y.test=y[test]##Test responses
##Fit the regression model, for all lambda, for the train data
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid, thresh=1e-12)
plot(lasso.mod)
##### TEST MSE for lambda[1]##########
lasso.pred1=predict(lasso.mod,s=lasso.mod$lambda[1] ,newx=x[test,])
mean((lasso.pred1-y.test)^2)#Test MSE for lambda[1]
predict(lasso.mod,s=lasso.mod$lambda[1] ,type="coefficients")[1:12,]
#####  Test MSE for lambda[78]  #########
lasso.pred2=predict(lasso.mod,s=lasso.mod$lambda[78] ,newx=x[test,]) 
mean((lasso.pred2-y.test)^2)
predict(lasso.mod,s=lasso.mod$lambda[78] ,type="coefficients")[1:12,]##The model

#Choosing best lambda using 5-folds cross validation Method on Train data

#Analaysis based on Train Data
set.seed(11)
x=model.matrix(Balance~.,Credit)[,-1]
y=Credit$Balance
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(Balance~., data=Credit[train, ])## Train data without response
test=(-train)
test.mat=model.matrix(Balance~., data=Credit[test, ])## Test data without response
y.test=y[test]##Test responses
##cv.glmnet has some dafault lambda values; 
#we may also set our own lambda values through grid
library(glmnet)
cv.out=cv.glmnet(x[train,],y[train],alpha=1, nfolds=5)
plot(cv.out)
#######  Chossing best Lambda  ###########
best.lambda=cv.out$lambda.min
best.lambda
##############  TEST MSE corresponding to best lambda
Lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=best.lambda, thresh=1e-12)
Lasso.pred4=predict(Lasso.mod,newx=x[test,]) 
mean((Lasso.pred4-y.test)^2)
########### Refit the model using the full data by using the lambda chosen
#by cross-validation technique
Full.mod=glmnet(x,y,alpha=1, lambda=best.lambda)
coef(Full.mod)

#Choosing best lambda using Cross validation on full data
set.seed(18)
x=model.matrix(Balance~.,Credit)[,-1]
y=Credit$Balance
grid=10^seq(10,-2,length=100)
grid
cv.full=cv.glmnet(x,y,alpha=1, lambda=grid, nfolds=10)
plot(cv.full)
best.lambda2=cv.full$lambda.min
best.lambda2
lasso.full=glmnet(x,y,alpha=1, lambda=best.lambda2)
coef(lasso.full)

# Fitting Elastic NET on a train data 
# and finding test mse on the test data 

set.seed(12)
x=model.matrix(Balance~.,Credit)[,-1]
y=Credit$Balance
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(Balance~., data=Credit[train, ])## Train data without response
test=(-train)
test.mat=model.matrix(Balance~., data=Credit[test, ])## Test data without response
y.test=y[test]##Test responses
##5-fold cross validation based on train data; we may set our own lambda=grid
list.fit=list() ####Creating empty list
for(i in 0:10){
  mod.alpha=paste("alpha", i/10)
  list.fit[[mod.alpha]]=cv.glmnet(x[train,],y[train],alpha=i/10, nfolds=5)
}
#TEST MSE calculation 
All_mod=data.frame()
for(i in 0:10){
  mod.alpha=paste("alpha", i/10)
 pred.net= predict(list.fit[[mod.alpha]],
                   s=list.fit[[mod.alpha]]$lambda.min, newx=x[test,]) 
mse.net=mean((y.test-pred.net)^2)
mse.list=data.frame(alpha=i/10, mse=mse.net, model.name=mod.alpha)
All_mod=rbind(All_mod,mse.list)
}
All_mod
min.mse=min(All_mod[,"mse"])
min.mse
All_mod[All_mod$mse==min.mse,]
#Final Model in elastic net based on the full data 
Final_model=glmnet(x,y,alpha=0.2, lambda=list.fit[["alpha 0.2"]]$lambda.min)
coef(Final_model)
###LSE Estimator 
#lasso.ls=predict(lasso.full,s=0 ,newx=x, exact=T) 
#mean((lasso.ls-y.test)^2)
#predict(lasso.full,s=0 ,type="coefficients")[1:12,]
