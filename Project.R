options(digits = 20)
#Importing dataset
data = read.csv('train.csv')
names(data)
head(data)
str(data)
new_d = data%>%select(-v.id)
x = data.matrix(new_d[,1:10])
y = new_d[c(11)]

x = scale(x, center = TRUE, scale = TRUE)
y = scale(y, center = TRUE, scale = TRUE)


# Create the training data and test data
set.seed(100) 
index = sample(1:nrow(x), 0.7*nrow(x)) 
x_train = x[index,] 
x_test = x[-index,] 

dim(x_train)
dim(x_test)

y_train = y[index,] 
y_test = y[-index,] 


#Training Linear Regression
train<-data.frame(y_train,x_train)
LReg<-lm(y_train~.,data =train)
summary(LReg)

#Prediction
xtest<-data.frame(x_test)
predict<-predict(LReg,xtest)

#MSE of Linear Regresssion Model
mse_lr<-mean((y_test - predict)^2)
mse_lr


#Create function for computing R squre and RMSE
Results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

eval_results(y_test, predict, x_test)



# RIDGE

library('glmnet')
ridge_reg <- glmnet(x_train, y_train, alpha = 0, lambda=10^seq(2, -3, by = -.1))
summary(ridge_reg)
cv.RIDGE = cv.glmnet(x_train,y_train,nfolds = 5, alpha=0,lambda=10^seq(2, -3, by = -.1))
optimal_lambda <- cv.RIDGE$lambda.min
optimal_lambda

predictions_r <- predict(ridge_reg, s = optimal_lambda, newx = x_test)
mse_r<-mean((y_test - predictions_r)^2)
mse_r
eval_results(y_test, predictions_r, x_test)

#LASSO 
lasso_reg <- glmnet(x_train, y_train, alpha = 1, lambda=10^seq(2, -3, by = -.1))
cv.LASSO = cv.glmnet(x_train,y_train,nfolds = 5, alpha=1)
best_lambda <- cv.LASSO$lambda.min
best_lambda

predictions_l <- predict(lasso_reg, s = best_lambda, newx = x_test)
mse_l<-mean((y_test - predictions_l)^2)
mse_l
eval_results(y_test, predictions_l, x_test)



#plot
par(mfrow=c(1,2))
plot(cv.LASSO)
title('LASSO')

plot(cv.RIDGE)
title('RIDGE')


outRIDGE = glmnet(x_train, y_train, alpha=0)
outLasso = glmnet(x_train, y_train)

out = glmnet(x_train, y_train,alpha = 0)
ridge.coef<-predict(ridge_reg, type = "coefficients", s = optimal_lambda)[1:11,]
ridge.coef

par(mfrow=c(1,2))
plot(outLasso)
title('Lasso')

plot(outRIDGE)
title('Ridge')


