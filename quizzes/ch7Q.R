# 7.1
load("sessions/7.R.RData")
plot(x,y)
fit<- lm(y~x)
summary(fit)

# 7.2
fit2 <- lm(y~x+I(x^2))
summary(fit2)