# 5.1
load("sessions/5.R.RData")
head(Xy)
model <- lm(y ~ X1 + X2, data = Xy)
summary(model)$coefficients
summary(model)$coefficients[2,2]

# 5.2
#Next, plot the data using matplot(Xy,type="l"). Which of the following do you think is most likely given what you see?
matplot(Xy,type="l")#Plot the columns of one matrix against the columns of another.
#Our estimate of s.e.(β^1) is too low. Our estimate of s.e.(β^1) is too low. - correct
#There is very strong autocorrelation between consecutive rows of the data matrix. Roughly speaking, we have about 10-20 repeats of every data point, so the sample size is in effect much smaller than the number of rows (1000 in this case).

# 5.3
library(boot)
alpha = function(x,y){
        vx = var(x)
        vy = var(y)
        cxy= cov(x,y)
        (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Xy$X1,Xy$y)

alpha.fn = function(data,index){
        with(data[index,],alpha(Xy$X1,Xy$y))
}

alpha.fn<-function(data, index) {
        fit1<-lm(y~., data=Xy[index,])
        coefficients(fit1)[['X1']]
}

set.seed(42)
alpha.fn (Xy,sample(1:100,100,replace=TRUE))

boot.out=boot(Xy,alpha.fn,R=1000)
boot.out


# 5.4
bs = function(data, index){
        x <- sample(seq(1, 901, 100), replace=T)
        indicies = as.vector(mapply(seq, from = x, to = x+99))
        summary(lm(y~., data=data[indicies, ]))$coefficients[2, 2]
}

boot(Xy, statistic=bs, R=10000)