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
#Prediction
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






#sheeet4

setwd("/Users/Local Drive/Teaching/2022/SMRA/R-CODES")
library(ISLR2)
#Default data
View(Default)
str(Default)
names(Default)
dim(Default)
summary(Default)
cor(Default[,c(-1,-2)])
attach(Default)
plot(Default)

#Logistic model

full.fit=glm(default~., data=Default,
            family=binomial)
summary(full.fit)
full.prob=predict(full.fit,Default,type="response")
full.prob
full.pred=rep("NO",nrow(Default))
full.pred
full.pred[full.prob>.5]="YES"
table(full.pred,Default[,1]) ##Confusion matrix

#Model Diagonosti
#COOK Distance
cook=cooks.distance(full.fit)
cook
plot(cook,type="b",pch=18,col="red")
n=10000
k=4
cutoff = 4/(n-k-1)
cutoff
abline(h=cutoff,lty=2)
for (i in 1:200)
  if(cook[i]>cutoff)
  {print(cook[i])}  
# Multicollinearity
car::vif(full.fit)

#logistic model on Train set 

train=sample(1:nrow(Default), nrow(Default)*0.80) 
train.mat=Default[train, ]# Train data
test=(-train)
test.mat=Default[test, ] # Test data
#Fitting the model on train set
logis.fit=glm(default~balance,data=Default,
             family=binomial, subset=train)
summary(logis.fit)
pred.prob=predict(logis.fit,test.mat,type="response")
pred.prob
library(pROC)
roc_score=roc(test.mat[,2], pred.prob)
roc_score
par(pty="s")##Margin set
plot(roc_score ,main ="ROC curve -- Logistic Regression ", legacy.axes=T)
plot(roc_score ,main ="ROC curve -- Logistic Regression ", legacy.axes=T,
     xlab="False positive rate", ylab="True positive rate")

# Fitting logistic model for  Glow 500 data (Full data)
library(aplore3)
View(glow500) ## This data avilable in aplore3 package
mod1 <- glm(fracture ~ age * priorfrac + height + momfrac * armassist +
                 I(as.integer(raterisk) == 3) ,
               family = binomial,
               data = glow500)
summary(mod1)
pred.mod1=predict(mod1,glow500,type="response")
pred.mod1
library(pROC)
roc_mod1=roc(glow500$fracture, pred.mod1) ## ROC score
roc_mod1
par(pty="s")##Margin set
plot(roc_mod1 ,main ="ROC curve -- Logistic Regression ", legacy.axes=T,
     xlab="False positive rate", ylab="True positive rate") ##ROC curve
#The same data with different model
mod2 <- glm(fracture ~ age + priorfrac + height + momfrac + armassist,
            family = binomial,
            data = glow500)
summary(mod2)
pred.mod2=predict(mod2,glow500,type="response")
pred.mod2
library(pROC)
roc_mod2=roc(glow500$fracture, pred.mod2) ## ROC score
roc_mod2
par(pty="s")##Margin set
plot(roc_mod2 ,main ="ROC curve -- Logistic Regression ", legacy.axes=T,
     xlab="False positive rate", ylab="True positive rate") ##ROC curve

#Subset Selection
library(mlbench)
data(PimaIndiansDiabetes2)
PimaIndiansDiabetes2=na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
#Fitting model using stepwise, forward and backward
full.model=glm(diabetes~., data=PimaIndiansDiabetes2, family=binomial)
summary(full.model)
null.model=glm(diabetes~1, data=PimaIndiansDiabetes2, family=binomial)
summary(null.model)
library(MASS)
stepAIC(full.model, direction="both")
stepAIC(full.model, direction="backward")
stepAIC(null.model, scope=list(lower=null.model, upper=full.model), direction="forward")
#
#Caution: The response variable should be in the last column.
#Factor variables with more than two levels should be 
#converted before running bestglm().
#Morgan-Tatar search means exhaustive serach.
library(leaps)
library(bestglm)
bestglm(PimaIndiansDiabetes2, IC="AIC", family=binomial, method = "forward")
bestglm(PimaIndiansDiabetes2, IC="BIC", family=binomial, method="exhaustive")

#Shrinkage Method
#THE LASSO

library(mlbench)
data(PimaIndiansDiabetes2)
PimaIndiansDiabetes2=na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
set.seed(11)
str(PimaIndiansDiabetes2)
x=model.matrix(diabetes~.,PimaIndiansDiabetes2,)[,-1]
y=PimaIndiansDiabetes2$diabetes
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(diabetes~., data=PimaIndiansDiabetes2[train, ])## Train data without response
test=(-train)
x.test=PimaIndiansDiabetes2[test,] ##test data with response
x.test
test.mat=model.matrix(diabetes~., data=x.test)[,-1]## Test data without response
test.mat #Test data without response
y.test=y[test]
y.test ##Test responses
#cv.glmnet has some dafault lambda values; 
#we may also set our own lambda values through grid
library(glmnet)
cv.out=cv.glmnet(x[train,],y[train],alpha=1, family="binomial", nfolds=5)
plot(cv.out)
#Chossing best Lambda
best.lambda=cv.out$lambda.min
best.lambda
#best Lasso model corresponding to min lambda
Lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=best.lambda, family="binomial")
coef(Lasso.mod)
#ROC for best model
#For model (fitted with glmnet function) test data should be without response
pred.prob=predict(Lasso.mod,test.mat,type="response")##test matrix should be withouit data
pred.prob
library(pROC)
roc_dia=roc(y.test, pred.prob) ## ROC score
roc_dia
par(pty="s")##Margin set
plot(roc_dia ,main ="ROC curve -- Logistic Regression ", legacy.axes=T,
     xlab="False positive rate", ylab="True positive rate") ##ROC curve
 #Refit the model using the full data  
Lasso.full=glmnet(x,y,alpha=1, lambda=best.lambda, family="binomial")
coef(Lasso.full)

#Comparing it with full model (i.e., with MLE model)

full.mod <- glm(diabetes~.,data=PimaIndiansDiabetes2,
            family = binomial,
            subset =train)
summary(full.mod)
#For model (fitted with glm function) test data should be with response
prob.full=predict(full.mod,x.test,type="response")
prob.full
library(pROC)
roc_full=roc(y.test, prob.full) # ROC score
roc_full
par(pty="s")#Margin set
plot(roc_mod1 ,main ="ROC curve -- Logistic Regression ", legacy.axes=T,
     xlab="False positive rate", ylab="True positive rate") #ROC curve

#Fitting Elastic NET on a train data

library(glmnet)
library(pROC)
library(mlbench)
data(PimaIndiansDiabetes2)
PimaIndiansDiabetes2=na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
set.seed(12)
x=model.matrix(diabetes~.,PimaIndiansDiabetes2,)[,-1]
y=PimaIndiansDiabetes2$diabetes
train=sample(1:nrow(x), nrow(x)*(2/3)) 
train.mat=model.matrix(diabetes~., data=PimaIndiansDiabetes2[train, ])## Train data without response
test=(-train)
x.test=PimaIndiansDiabetes2[test,] ##test data with response
test.mat=model.matrix(diabetes~., data=x.test)[,-1]## Test data without response
test.mat #Test data without response
y.test=y[test] #Test responses
#
#5-fold cross validation based on train data; we may set our own lambda=grid
#For each alpha, we get the best model corresponding to best lambda 
list.fit=list() #Creating empty list
for(i in 0:10){
  mod.alpha=paste("alpha", i/10)
  list.fit[[mod.alpha]]=cv.glmnet(x[train,],y[train],alpha=i/10, family="binomial", nfolds=5)
}
#Binomial deviation calculation
#Fitting the elastic net corresponding to each alpha (and its corresponding best lambda)
elastic.mod=list()
for(i in 0:10){
  mod.alpha=paste("alpha", i/10)
  elastic.mod[[mod.alpha]]=glmnet(x[train,],y[train],
                                alpha=i/10,lambda=list.fit[[mod.alpha]]$lambda.min, family="binomial") 
}

#ROC measure corresponding to each alpha (and its corresponding best lambda)
roc.list=data.frame()
All_mod=data.frame()
for(i in 0:10){
  mod.alpha=paste("alpha", i/10)
  pred.net= predict(elastic.mod[[mod.alpha]], test.mat, type="response") 
  roc.net=auc(roc(y.test, pred.net))
  roc.list=data.frame(alpha=i/10, ROC=roc.net, model.name=mod.alpha)
  All_mod=rbind(All_mod,roc.list)
}
All_mod
max.ROC=max(All_mod[,"ROC"])
max.ROC
All_mod[All_mod$ROC==max.ROC,]
#Final Model in elastic net based on the full data
Final_model=glmnet(x,y,alpha=0.2, lambda=list.fit[["alpha 0.2"]]$lambda.min, family="binomial")
coef(Final_model)

#Multinomial Logiistic regression for tissue data

library(readxl)
tissue_Des= read_excel("BreastTissue.xls", sheet="Description")
View(tissue_Des)
tissue= read_excel("BreastTissue.xls", sheet="Data")
View(tissue)
str(tissue)
#
library(nnet)
#tissue <- tissue[, -1]
tissue$Class <- as.factor(tissue$Class)#changing character to factor variable
str(tissue)
#We merge the fibro-adenoma, mastopathy, and glandular classes 
#as their discrimination are not important and hence, 
#combining the levels "fad", "gla" and "mas" as a single level "others"
levels(tissue$Class)[levels(tissue$Class) %in% c("fad", "gla", "mas")] <- "other"
levels(tissue$Class)

#Create a training set
train=sample(1:nrow(tissue), nrow(tissue)*0.70) 
train.mat=tissue[train, ]## Train data
test=(-train)
test.mat=tissue[test, ]## Test data
# Setting the reference
train.mat$Class <- relevel(train.mat$Class, ref = "adi")
library(nnet)
# Training the multinomial model
multinom_model <- multinom(Class ~ ., data = train.mat)# Checking the model
summary(multinom_model)
#### fitted probabilities for first six observations
#head(round(fitted(multinom_model), 2))
#Predicting the class for test dataset
test.pred <- predict(multinom_model, newdata = test.mat, type="class")
test.pred
# Building classification table
tab2 <- table(test.mat$Class, test.pred)
tab2
(sum(diag(tab2))/sum(tab2))*100

#Multinomial logistic regression
# For iris data 

iris=read.csv("iris.csv")
View(iris)
names(iris)
dim(iris)
str(iris)
summary(iris)
cor(iris[,-5])
iris$variety <- as.factor(iris$variety)#changing character to factor variable
str(iris)
#Create a training set
set.seed(9)
train=sample(1:nrow(iris), nrow(iris)*0.70) 
train.mat=iris[train, ]## Train data
test=(-train)
test.data=iris[test, ]## Test data
y.test=iris$variety[test]
y.test
# Setting the reference
train.mat$variety <- relevel(train.mat$variety, ref = "Setosa")
library(nnet)
# Training the multinomial model
multinom_model2 <- multinom(variety ~ ., data = train.mat)# Checking the model
summary(multinom_model2)
#Predicting the class for test dataset
test.prob= predict(multinom_model2, newdata = test.data, type="probs")
test.prob
test.class= predict(multinom_model2, newdata = test.data, type="class")
test.class
# Building classification table
tab3 <- table(test.data$variety, test.class)
tab3
(sum(diag(tab3))/sum(tab3))*100
#library(pROC)
#mul.roc=multiclass.roc(test.class,as.numeric(y.test))
#mul.roc
#mul.roc$rocs[[1]]
#mul.roc$rocs[[2]]
#mul.roc$rocs[[3]]
#plot.roc(mul.roc$rocs[[1]] ,print.auc=T, legacy.axes=T,
#     xlab="False positive rate", ylab="True positive rate")
#plot.roc(mul.roc$rocs[[2]] ,print.auc=T, col="red", add=T, legacy.axes=T, print.auc.adj = c(0,3))
#plot.roc(mul.roc$rocs[[3]] ,print.auc=T, col="green", add=T, legacy.axes=T, print.auc.adj = c(0,5))

#Poisson regression
#Bikeshare data

library(ISLR2)
View(Bikeshare)
names(Bikeshare)
dim(Bikeshare)
str(Bikeshare)
## Poission regression on fulldata
mod.pois <- glm(bikers~ mnth + hr + workingday + temp + weathersit, 
                data = Bikeshare, family = poisson)
summary(mod.pois)
#
plot(coef(mod.pois)[2:12], xlab="Month", ylab="coefficient",
     xaxt="n", col="blue", pch=19, type="o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
plot(coef(mod.pois)[1:23], xlab = "Hour", ylab = "Coefficient", 
     col = "blue", pch = 19, type = "o")





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
#
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
#
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
#
plot(reg.summary$cp, xlab = "Number of Variables",
     ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points (6, reg.summary$cp[6] , col = "red", cex = 2,
        pch = 20)
#
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
