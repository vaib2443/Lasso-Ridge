#loading libraries
library(glmnet)
library(Metrics)
library(ISLR)
library(ggplot2)
library(caret)
library(stats)

################################################
#loading data set and renaming it as colg
################################################
colg<-College

################################################
#Splitting the data set into train and test
################################################

#set seed to fix randomization
set.seed(123)
#creating partition in the data set and splitting it in 80-20
traindata<-createDataPartition(colg$Private, p=0.80, list = F)
#creating training data to include 80% rows
train<-colg[traindata,]
#creating test data to include 20% rows
test<-colg[-traindata,]

#putting the data into a matrix
#we are using model.matrix function will dummy code the categorical values and we are splitting the data in train and test while remove the prediction variable, in this data, it's last column of Grad.Rate 
train_x<-model.matrix(Grad.Rate~., train)[,-18]
#using model.matrix to create the test data without the output variable
test_x<-model.matrix(Grad.Rate~., test)[,-18]
#creating the vector variable for training set and assigning it to train_y
train_y<-train$Grad.Rate
#creating the vector variable for testing set and assigning it to test_y
test_y<-test$Grad.Rate

################################################
#ridge regression (L2)
################################################

#setting seed value to fix randomization
set.seed(12345)

################################################
#finding the right value of lambda (ridge)
################################################
#using cv.glmnet function with alpha value 0 to perform K-Cross cross validation and find the optimum value of lambda
cv.ridge<-cv.glmnet(train_x,train_y,alpha=0, nfolds = 10)

#optimal value of lambda that minimizes the error 
#lambda.min (λ) is the minimum mean cross-validated error
cv.ridge$lambda.min
#lambda.1se is the largest possible value of λ  that within 1 standard error of lambda.min.
cv.ridge$lambda.1se

#plotting the cross-validation cv.ridge object
plot(cv.ridge)

########################################################
#fitting the ridge(L1) regression model based on lambda
########################################################

#ridge regression model based on lambda.min
model.min.L2<-glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.min)
model.min.L2

#regression coefficient of lambda.min
coef(model.min.L2)

#ridge regression model based on lambda.1se
model.1se.L2<-glmnet(train_x, train_y, alpha = 0, lambda = cv.ridge$lambda.1se)
model.1se.L2

#regression coefficient of lambda.1se
coef(model.1se.L2)

########################################################
#setting prediction on training set (ridge)
########################################################
pred.train.L2<-predict(model.1se.L2, newx = train_x)
#calculating Root Mean Squared Error to measure the performance of training set of ridge regression
train.rmse.L2<-rmse(train_y, pred.train.L2)
#rmse of ridge regression training set
train.rmse.L2

########################################################
#setting prediction on testing set (ridge)
########################################################
pred.test.L2<-predict(model.1se.L2, newx = test_x)
#calculating Root Mean Squared Error to measure the performance of testing set of ridge regression
test.rmse.L2<-rmse(test_y, pred.test.L2)
#rmse of ridge regression training set
test.rmse.L2

################################################
#Lasso regression (L1)
###############################################

#setting seed value to fix randomization
set.seed(12345)

################################################
#finding the right value of lambda
################################################
#using cv.glmnet function with alpha value 1 to perform K-Cross cross validation and find the optimum value of lambda
cv.lasso<-cv.glmnet(train_x,train_y,alpha=1, nfolds = 10)
#plotting the cross-validation cv.lasso object
plot(cv.lasso)

#optimal value of lambda that minimizes the error 
#lambda.min (λ) is the minimum mean cross-validated error
cv.lasso$lambda.min
#lambda.1se is the largest possible value of λ  that within 1 standard error of lambda.min
cv.lasso$lambda.1se

########################################################
#fitting the Lasso(L1) regression model based on lambda
########################################################
#lasso regression model based on lambda.min
model.min.L1<-glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.min)
model.min.L1

#regression coefficient of lambda.min
coef(model.min.L1)

#lasso regression model based on lambda.1se
model.1se.L1<-glmnet(train_x, train_y, alpha = 1, lambda = cv.lasso$lambda.1se)
model.1se.L1

#regression coefficient of lambda.1se
coef(model.1se.L1)

########################################################
#setting prediction on train data set (Lasso)
########################################################
pred.train.L1<-predict(model.1se.L1, newx = train_x)
#calculating Root Mean Squared Error to measure the performance of training set of lasso regression
train.rmse.L1<-rmse(train_y, pred.train.L1)
#rmse value of training set(lasso)
train.rmse.L1

########################################################
#setting prediction on test set (lasso)
########################################################
pred.test.L1<-predict(model.1se.L1, newx = test_x)
#calculating Root Mean Squared Error to measure the performance of testing set of lasso regression
test.rmse.L1<-rmse(test_y, pred.test.L1)
#rmse value of test set(lasso)
test.rmse.L1

#compare ridge and lasso model
test.rmse.L2
test.rmse.L1

########################################################
#stepwise Selection Model
########################################################
#using both direction stepwise regression
step_select <- step(lm(Grad.Rate~.,data=colg),direction="both")
#we want to see the p-values and estimates of our model
summary(step_select)

# fitting linear model using lowest AIC value from the stepwise regression model
lm_step<-lm(data = train, Grad.Rate ~ Private + Apps + Top25perc + P.Undergrad + Outstate + Room.Board + Personal + PhD + Terminal + perc.alumni + Expend)
#we want to see the p-values and estimates of our final model
summary(lm_step)

########################################################
#setting prediction on train set (stepwise)
########################################################
lm_step_train<-predict(lm_step, newdata =train)
#calculating Root Mean Squared Error to measure the performance of training set of stepwise regression
train.rmse.step<-rmse(train_y, lm_step_train)
#rmse of training set(stepwise)
train.rmse.step

########################################################
#setting prediction on test set (stepwise)
########################################################
lm_step_test<-predict(lm_step, newdata=test)
#calculating Root Mean Squared Error to measure the performance of testing set of stepwise regression
test.rmse.step<-rmse(test_y, lm_step_test)
#rmse of test set(stepwise)
test.rmse.step

########################################################
#displaying the best model in a table
########################################################

#Naming rows
row=c("Ridge","Lasso","Stepwise")

#Creating Data frame with train and test rmse value
table<-data.frame("RMSE_Train"=c(train.rmse.L2,train.rmse.L1,train.rmse.step),"RMSE_Test"=c(test.rmse.L2,test.rmse.L1,test.rmse.step))

#adding rows to table
row.names(table)<-row

#display table
kable(table, caption  = "Comparing Models")
