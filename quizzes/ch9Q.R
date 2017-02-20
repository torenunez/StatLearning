library(stats)
library(MASS)
library(e1071)

x0_train = MASS::mvrnorm(50, rep(0,10), diag(10)) 
x1_train = MASS::mvrnorm(50, c(rep(1,5),rep(0,5)), diag(10)) 
x_train = rbind(x0_train,x1_train)
y_train = rep(c(0,1), c(50,50)) 
train_data = data.frame(x_train, y = as.factor(y_train))

x0_test = MASS::mvrnorm(500, rep(0,10), diag(10)) 
x1_test = MASS::mvrnorm(500, c(rep(1,5),rep(0,5)), diag(10)) 
x_test = rbind(x0_test,x1_test)
y_test = rep(c(0,1), c(500,500)) 
test_data = data.frame(x_test, y = as.factor(y_test))

sample_train <- train_data[sample(1:nrow(train_data), 500, replace=TRUE),]
miss_class <- function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}


# Q9.1 
svm_m1 = e1071::svm(sample_train$y ~., data=sample_train)
svm_pred1 <- predict(svm_m1, newdata = test_data)
(sum(svm_pred1 != test_data$y))/length(test_data$y)


# Q9.2
svm_m2 = e1071::svm(sample_train$y ~., data=sample_train, kernel = "linear")
svm_pred2 <- predict(svm_m2, newdata = test_data)
(sum(svm_pred2 != test_data$y))/length(test_data$y)


# Q9.3
logis_m = stats::glm(sample_train$y ~., family = "binomial", data=sample_train)
logis_pred <- predict(logis_m, newdata=test_data, type = 'response')
miss_class(test_data$y,logis_pred)