rm(list=ls())
setwd("C:/Users/Shilpa/Desktop/TAMU/Sem 2/APA/project") 

pt <- read.csv('parkinsons_updrs.data', header = TRUE)
#View(pt)

str(pt)

library(corrplot)
corrplot(cor(pt), type = 'lower', method = "circle")

pt <- data.frame(pt[,-5])# removing motor_UPDRS
pt <- data.frame(pt[,-1]) # removing subject number

corrplot(cor(pt), type = 'lower', method = "circle")

set.seed(25)

index = sample(1:nrow(pt), as.integer(0.7 *nrow(pt)))

pt_train = pt[index,]
pt_test = pt[-index,]

#Training the data

library(randomForest)

rf <- randomForest(total_UPDRS~., data = pt_train, importance = TRUE)
rf
summary(rf)
pred <- predict(rf, pt_test, type = 'response')
sqrt(mean((pred-pt_test$total_UPDRS)^2)) 
plot(pt_test$total_UPDRS,pred,  xlab = 'Observed', ylab = "Predicted")

#Tuning on number of predictors
mse = rep(0,19)
for(vars in c(1:19)){
  set.seed(25)
  rfloop <- randomForest(total_UPDRS~., data = pt_train, mtry = vars, importance = TRUE)
  predloop <- predict(rfloop, pt_test, type = 'response')
  mse[vars] <- sqrt(mean((predloop-pt_test$total_UPDRS)^2))
}

mse

min(mse)

which.min(mse)


#Bagging as it has min mse
set.seed(25)
rf2 <- randomForest(total_UPDRS~., data = pt_train, mtry = 19, importance = TRUE)
rf2
pred2 <- predict(rf2, pt_test, type = 'response')
sqrt(mean((pred2-pt_test$total_UPDRS)^2))
plot(pt_test$total_UPDRS,pred2,  xlab = 'Observed', ylab = "Predicted")

#bagging with 1000 trees
set.seed(25)
rf3 <- randomForest(total_UPDRS~., data = pt_train, mtry = 19, ntree = 1000,importance = TRUE)
pred3 <- predict(rf3, pt_test, type = 'response')
mean((pred3-pt_test$total_UPDRS)^2)

#bagging with 100 trees
set.seed(25)
rf4 <- randomForest(total_UPDRS~., data = pt_train, mtry = 19, ntree = 100,importance = TRUE)
pred4 <- predict(rf4, pt_test, type = 'response')
mean((pred4-pt_test$total_UPDRS)^2)

# Assessing variable importance
importance(rf2)
varImpPlot(rf2)
