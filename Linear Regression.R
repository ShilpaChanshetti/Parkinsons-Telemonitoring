# *************** Linear Regression on Parkinsons Telemonitoring disease********************

# Loading the data
park <- read.csv("parkinsons_updrs.data", sep = ",")
attach(park)

# Removing any NA values in the data
park <- na.omit(park)

# Removing the number of rows with negative values of test_time
park <- park[park$test_time >= 0, ]

parkdata <- park[,-c(1,5)]

set.seed(25)
index = sample(1:nrow(parkdata), round(0.70 *nrow(parkdata)))
parkdata_train = parkdata[index,]
parkdata_test = parkdata[-index,]

Lin_results<-lm(total_UPDRS~., parkdata_train, na.action = na.exclude)
summary(Lin_results)

par (mfrow=c(2,2)) # will show 4 plots in one graph as a 2x2 matrix
plot(Lin_results) 

predUPDRS=predict(Lin_results,parkdata_test) # uses the model to predict values
par(mfrow=c(1,1)) # shows one plot in the plot window
par(pch=20, col="black") # plotting symbol and color
plot(parkdata_test$total_UPDRS,predUPDRS,type='p', xlab="obs UPDRS", ylab="predicted UPDRS")
lines(parkdata_test$total_UPDRS,parkdata_test$total_UPDRS)


sqrt(mean((parkdata_test$total_UPDRS-predUPDRS)^2,na.rm=TRUE)) #9.7909
predict(Lin_results,parkdata_test,interval="confidence")
predict(Lin_results,parkdata_test,interval="prediction")


################# BEST SUBSET SELECTION ###################################
install.packages("leaps")
library(leaps)
regfit.full=regsubsets(total_UPDRS~.,parkdata)

summary(regfit.full)

regfit.full=regsubsets(total_UPDRS~.,data=parkdata,nvmax=20)

reg.summary=summary(regfit.full)

names(reg.summary)
reg.summary$rsq

############ Forward and Backward Stepwise Selection ##############

regfit.fwd=regsubsets(total_UPDRS~.,data=parkdata,nvmax=20,method="forward")
summary(regfit.fwd)

regfit.bwd=regsubsets(total_UPDRS~.,data=parkdata,nvmax=20,method="backward")
summary(regfit.bwd)

coef(regfit.full,10)
coef(regfit.fwd,10)
coef(regfit.bwd,10)

#################LASSO########################################
library(leaps)

x=model.matrix(total_UPDRS~.,parkdata)[,-c(1,5)]
y=parkdata$total_UPDRS
library(glmnet)

set.seed(25)
train=sample(c(TRUE,FALSE), nrow(parkdata),rep=TRUE)
test=(!train)
y.test=y[test]


cv.out=cv.glmnet(x[train,],y[train],alpha=1)
bestlam=cv.out$lambda.min
bestlam

plot(cv.out)

out=glmnet(x,y,alpha=1)
predict(out,type="coefficients",s=bestlam)[1:19,]

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=bestlam)
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
sqrt(mean((lasso.pred-y.test)^2))


par(mfrow=c(1,1)) # shows one plot in the plot window
par(pch=20, col="black") # plotting symbol and color
plot(y.test,lasso.pred,type='p', xlab="obs UPDRS", ylab="predicted UPDRS")
lines(y.test,y.test)

