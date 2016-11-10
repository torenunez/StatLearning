load("sessions/5.R.RData")
head(Xy)
model <- lm(y ~ X1 + X2, data = Xy)
summary(model)$coefficients
summary(model)$coefficients[2,2]

#Next, plot the data using matplot(Xy,type="l"). Which of the following do you think is most likely given what you see?
matplot(Xy,type="l")#Plot the columns of one matrix against the columns of another.
#Our estimate of s.e.(β^1) is too low. Our estimate of s.e.(β^1) is too low. - correct
#There is very strong autocorrelation between consecutive rows of the data matrix. Roughly speaking, we have about 10-20 repeats of every data point, so the sample size is in effect much smaller than the number of rows (1000 in this case).


b1 <- function(data){
        model <- lm(y ~ X1 + X2, data = data)        
        se <- summary(model)$coefficients[2,2]
        return(se)
}


b1s=function(data, index){
        with(data[index,],b1(data)) 
}

b1s(Xy,1:100)

set.seed(1)
b1s(Xy,sample(1:1000,1000,replace=TRUE))


boot.out=boot(Xy,b1s,R=1000)
