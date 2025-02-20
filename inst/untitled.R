setwd("/Users/Local Drive/Teaching/2025/SMRA/R-CODES")
adv=read.csv("Advertising.CSV", header=T,stringsAsFactors=T)
adv
summary(adv)
%%%%%%%%%%%%%%%%%%  Matrix Scatter Plot  %%%%%%%%%%%%%%%%%%%%%
pairs(~Sales+TV+Radio+Newspaper,adv)
%%%%%%%%%%%%%%%%%%%  Model-1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lim1.fit=lm(Sales~TV, data=adv)
summary(lim1.fit)
confint(lim1.fit, level=0.97)
%%%%%%%%%%%%%%%%%%  Model-II  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lim2.fit=lm(Sales~TV+Radio, data=adv)
summary(lim2.fit)
confint(lim2.fit, level=0.98)
%%%%%%%%%%%%%%%%%%%%  Model-III  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lim3.fit=lm(Sales~TV+Radio+Newspaper, data=adv)
summary(lim3.fit)
confint(lim3.fit, level=0.96)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Estimate of the mean response Model-II %%%%%%%%%%%%%%%%%
  predict(object = lim2.fit, newdata = data.frame(TV =100, Radio=20), 
          interval = "confidence", level=.95)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Prediction of new observation for Model-II %%%%%%%%%%%%%%%%%
  predict(object = lim2.fit, newdata = data.frame(TV =100, Radio=20), 
          interval = "prediction", level=.95)
##  Model Adequacy Checking ####PRESS ############################################
x=model.matrix(lim2.fit)
PRESS_res=summary(lim2.fit)$res/(1-hat(x))
print(PRESS_res)
##############  Plotting of PRESS Residuals #########################
par(mfrow=c(1,1))
plot(PRESS_res,ylab="PRESS residual")
################  R^2 Prediction #######################
PRESS=sum(PRESS_res^2)
TSS= sum(anova(lim2.fit)$"Sum Sq")
pred.r.squared = 1 - PRESS/(TSS)
pred.r.squared
  %%%%%%%%%%%%%%%%%%%  Model diagonostic Cheking: Outlier Detection  %%%%%%%%%%%%%%%%%%%
rstandard(lim2.fit)
rstudent(lim2.fit)   ## Studeentized residual
par(mfrow=c(2,2))  ##one plot in a given page
plot(rstandard(lim2.fit),ylab="Standarized residual")
plot(rstudent(lim2.fit),ylab="Studentized residual")
########################### Leverage Points ##########
h=hatvalues(lim2.fit)
h
k=2
n=200
t=2*k/n
for (i in 1:200)
if(h[i]>t)
{print(h[i])}
######################  Cook's distance ##############
cook=cooks.distance(lim2.fit)
cook
plot(cook,type="b",pch=18,col="red")
n=200
k=2
cutoff = 4/(n-k-1)
cutoff
abline(h=cutoff,lty=2)
for (i in 1:200)
  if(cook[i]>cutoff)
  {print(cook[i])}  
###################   DFBETAS    ################
def=dfbetas(lim2.fit)
def
###################   DFFITS    ################
dff=dffits(lim2.fit)
abs(dff)
plot(abs(dff),type="b",pch=18,col="red")
n=200
k=2
cutoff = 2*sqrt(k/n)
cutoff
abline(h=cutoff,lty=2)
for (i in 1:200)
  if(abs(dff)[i]>cutoff)
  {print(abs(dff)[i])} 
######################   Covratio  ###############
cv=covratio(lim2.fit)
cv
n=200
k=2
for (i in 1:n)
  if(cv[i]>1+3*k/n|cv[i]<1-3*k/n)
  {print(cv[i])}
#####################Durbin-watson test ##############
library(DescTools)
DurbinWatsonTest(lim2.fit, alternative ="less")
DurbinWatsonTest(lim2.fit, alternative ="greater")
DurbinWatsonTest(lim2.fit, alternative ="two.sided")
################  Heteroscedasticity (Non-Constant Variance) #############
par(mfrow=c(2,2))
plot(lim2.fit$fit,lim2.fit$res,xlab="fitted", ylab="Residual")
abline(h=0)
##%%%%%%%%%%%%%%%%%  Normality Checking %%%%%%%%%%%%%%%%%%%%%%%%
##################  Q-Q Plot  ###########
dev.off()  ###### This clears all existing plots from the Rstudio.
par(mar=c(2,2,2,0))
qqnorm(rstudent(lim2.fit))
qqline(rstudent(lim2.fit), col='red')## Q-Q Plot
########p-p Plot %%%%%%%%%%%%%
library(faraway)
par(mar=c(2,2,2,0))
probDist <- pnorm(rstudent(lim2.fit))
plot(ppoints(length(rstudent(lim2.fit))), sort(probDist),
     main = "PP Plot_studentized", xlab = "Observed Probability", 
     ylab = "Expected Probability") 
abline(0,1)#add diagonal line
############ BOX-COX transformation ########
# library(MASS)
# #find optimal lambda for Box-Cox transformation 
# par(mar=c(1,1,1,1))##Fix the margin
# bc <- boxcox(lim2.fit, data=adv,  lambda=seq(-.2, 0.5, length = 10))
# ##names(bc)
# lambda <- bc$x[which.max(bc$y)]
# lambda
# #fit new linear regression model using the Box-Cox transformation
# new_model <- lm(((Sales^lambda-1)/lambda) ~ TV+Radio, data=adv)
# summary(new_model)
#################################################################################
##################   BOX-COX Transformation #####################################
#################################################################################
library(car)
###################  bcPower transformation for positive response  ###############
max(adv$Sales)/min(adv$Sales)
p1=powerTransform(Sales~TV+Radio+Newspaper, family="bcPower",data=adv)
summary(p1)
#coef(p1, round=TRUE)
newmodel= lm(bcPower(Sales, p1$roundlam, jacobian.adjusted = TRUE) ~ TV+Radio+Newspaper,
             data=adv)
summary(newmodel)
########################  bcnPower transformation for negative responses #########
credit=read.csv("Credit-2.CSV", header=T,stringsAsFactors=T)
credit ###The credit data
mod_cr=lm(Balance~Income+Limit, data=credit)
summary(mod_cr)###Simple linear model
p2=powerTransform(Balance~Income+Limit, family="bcnPower",data=credit)
summary(p2)##Box-cox transformation summary
#coef(p2, round=TRUE)
newmodel2= lm(bcnPower(Balance, p2$roundlam, jacobian.adjusted = TRUE, gamma=413.97) 
              ~ Income+Limit,data=credit)
summary(newmodel2)
######################## Yeo-Jhonson Power TRansformation for negative response  #####
p3=powerTransform(Balance~Income+Limit, family="yjPower",data=credit)
summary(p3)
#coef(p3, round=TRUE)
newmodel3= lm(yjPower(Balance, p3$roundlam) ~ Income+Limit, data=credit)
summary(newmodel3)
#####################################################################################
################# Partial regression and Partial residual plots #####################
#####################################################################################
library(car)
model=lm(Sales~TV+Radio+Newspaper,data=adv)
avPlots(model)##Partial regression plot
###########################################
library(faraway)
model=lm(Sales~TV+Radio+Newspaper,data=adv)
par(mfrow=c(2,2))
prplot(model, 1)
prplot(model, 2)
prplot(model, 3)
######################  Multicolinearity ############
View(adv)
X<-adv[,2:4]
library(GGally)
ggpairs(X) ##Plotting of correlation matrix 
#library(corpcor)
#cor2pcor(cov(X))##Correlation matrix
#############
library(mctest)
omcdiag(model)
imcdiag(model, method="VIF", corr="T")
######### Pattern of the multicollinearity
library(ppcor)
pcor(X, method = "pearson")


################################
library(ISLR)
library(MASS)
data(Credit)
View(Credit)
write.csv("Credit")
write.csv(Credit, file = "Credit.csv")
#############################
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  Subset selection  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  library(MASS)
step.model <- stepAIC(lim3.fit, direction = "both", 
                      trace = FALSE)  %%
  summary(step.model)
%%%%%%%%%%%%%%%%%%%%
  stepf.model <- stepAIC(lim3.fit, direction = "forward", trace = FALSE)
summary(stepf.model)
stepb.model <- stepAIC(lim3.fit, direction = "backward", trace = FALSE)
summary(stepb.model)
%%%%
  adv=read.csv("Advertising.CSV", header=T,stringsAsFactors=T)
  adv
  summary(adv)
  #fit linear regression model
  model <- lm(Sales~TV+Radio+Newspaper, data=adv)
  summary(model)
  par(mfrow=c(2,2))
  plot(model)
  max(adv$Sales)/min(adv$Sales)