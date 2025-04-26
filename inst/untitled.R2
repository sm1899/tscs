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

