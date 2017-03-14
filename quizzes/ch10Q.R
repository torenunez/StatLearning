load("sessions/10.R.RData")

# 10.R.1 
X_all = rbind(x, x.test)
pca=prcomp(X_all, scale=TRUE)
summary(pca)$importance
summary(pca)$importance[,5]

# 10.R.2
library(Metrics)
transformation <- pca$rotation[,1:5]

X_pca_train <- as.matrix(x) %*% transformation
X_pca_test <- as.matrix(x.test) %*% transformation
train_data <- data.frame(xvals = X_pca_train, y = y)
test_data <- data.frame(xvals = X_pca_test, y = y.test)

fit <- lm(y ~ ., data=train_data)
y_pred <- predict(fit, newdata=test_data)

mse(y.test,y_pred)


# 10.R.3
X_train <- data.frame(x,y=y)
X_test <- data.frame(x.test, y=y.test)
fit <- lm(y ~ ., data = X_train)
y_pred2 <- predict(fit, newdata = X_test)
print(mse(y.test, y_pred2))

