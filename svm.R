rm(list=ls())

pt <- read.csv('parkinsons_updrs.data', header = TRUE)

pt <- data.frame(pt[,-5])# removing motor_UPDRS
pt <- data.frame(pt[,-1]) # removing subject number

corrplot(cor(pt), type = 'lower', method = "circle")

set.seed(25)

index = sample(1:nrow(pt), as.integer(0.7 *nrow(pt)))

pt_train = pt[index,]
pt_test = pt[-index,]

library(e1071)
#linear
tune.out=tune(svm,total_UPDRS~.,data=pt_train,kernel="linear",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10),epsilon=c(0.1,0.5,1)))
summary(tune.out) 
bestmod=tune.out$best.model
summary(bestmod) 
predict2 <- predict(bestmod,pt_test, type= 'response')
svmRMSE2 <- sqrt(mean((predict2-pt_test$total_UPDRS)^2))
svmRMSE2

plot(pt_test$total_UPDRS,predict2, xlab = "Observed",ylab = "Predicted")

#radial
tune.out=tune(svm,total_UPDRS~.,data=pt_train,kernel="radial",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10),gamma=c(0.1,0.3,0.4,0.5,1,2)))
summary(tune.out) 
bestmod=tune.out$best.model
summary(bestmod)
predict2 <- predict(bestmod,pt_test, type= 'response')
svmRMSE2 <- sqrt(mean((predict2-pt_test$total_UPDRS)^2))
svmRMSE2

plot(pt_test$total_UPDRS,predict2, xlab = "Observed",ylab = "Predicted")

#poly
tune.out=tune(svm,total_UPDRS~.,data=pt_train,kernel="polynomial",ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10),degree=c(2,3,4)))
summary(tune.out)
bestmod=tune.out$best.model
summary(bestmod)
predict2 <- predict(bestmod,pt_test, type= 'response')
svmRMSE2 <- sqrt(mean((predict2-pt_test$total_UPDRS)^2))
svmRMSE2 
plot(pt_test$total_UPDRS,predict2, xlab = "Observed",ylab = "Predicted")
